{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bitcoin.Core.Regtest.Generator (
    Funding (..),
    generateWithTransactions,
) where

import Bitcoin.Core.RPC (
    BitcoindClient,
    ListUnspentOptions (ListUnspentOptions),
 )
import qualified Bitcoin.Core.RPC as RPC
import Bitcoin.Core.Regtest.Framework (NodeHandle, oneBitcoin, runBitcoind)
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Exception (throwIO)
import Control.Monad (foldM, forever, replicateM_, unless, when, (>=>))
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (
    StateT (StateT),
    evalStateT,
    gets,
    mapStateT,
    modify',
 )
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.List (uncons, unfoldr)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Serialize as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin (
    Script (Script),
    TxHash,
    TxIn (TxIn),
 )
import qualified Haskoin as H
import Haskoin.Block (BlockHeight)
import Haskoin.Transaction (
    OutPoint (..),
    Tx (..),
    TxOut (..),
 )
import Network.HTTP.Client (Manager)

-- | How much to pay out on each faucet request
faucetPayment :: Word64
faucetPayment = oneBitcoin

-- | If the series count exceeds this value, stop generating series
maxSeries :: Int
maxSeries = seriesLength * seriesCount

-- | The number of series in a group
seriesCount :: Int
seriesCount = length outputsPerSeries

-- | How many transactions in a series to broadcast
seriesLength :: Int
seriesLength = 10

-- | Initial amount per output
amountPerOutput :: Word64
amountPerOutput = 20000

-- | This value determines the transaction series that will fill up the mempool
outputsPerSeries :: [Word64]
outputsPerSeries =
    mconcat
        [ replicate 20 100 -- 20 txs with 100 outputs
        , replicate 30 5 -- 30 txs with 5 outputs
        , replicate 100 2 -- 100 txs with 2 outputs
        ]

-- | Funding configuration
data Funding
    = -- | Send 1BTC each time the action produces an address
      Faucet (IO (Maybe Text))
    | -- | Send a certain amount of bitcoin once
      OneTime [(Text, Word64)]

-- | Generate many transactions per block with a certain mean fee rate
generateWithTransactions ::
    Manager ->
    NodeHandle ->
    -- | Block interval (seconds per block)
    Int ->
    -- | Optional funding behavior
    Maybe Funding ->
    -- | Mean fee rate at height
    (BlockHeight -> Word64) ->
    IO ()
generateWithTransactions mgr nodeHandle blockInterval funding getMeanFeeRate = do
    run initialize

    (async >=> link) . run . RPC.withWallet miningWallet $ case funding of
        Just (Faucet getExternalAddress) ->
            waitRepeat 1 $ do
                balance <- getBalance
                when (balance >= faucetPayment) $
                    liftIO getExternalAddress >>= mapM_ (fundAddress faucetPayment)
        Just (OneTime payouts) -> mapM_ (uncurry handlePayout) payouts
        _ -> pure ()

    run . flip evalStateT mempty . waitRepeat blockInterval $ do
        mapStateT (RPC.withWallet floodingWallet) $ txFlood getMeanFeeRate
        blockHash <- lift . generatorGenerate =<< getSeriesCount
        liftIO . putStrLn $ "New block: " <> show blockHash
  where
    run = runBitcoind mgr nodeHandle >=> either throwIO pure

    miningWallet = "bitcoin-regtest-generator.mine"
    floodingWallet = "bitcoin-regtest-generator.flood"

    newWallet name = do
        RPC.createWallet
            name
            Nothing
            Nothing
            mempty
            Nothing
            Nothing
            Nothing
            Nothing

    initialize = do
        newWallet miningWallet
        newWallet floodingWallet

        RPC.withWallet miningWallet . sweep . (* 15) . getMeanFeeRate =<< RPC.getBlockCount
        replicateM_ 5 $ generatorGenerate 0

        balance <- RPC.withWallet miningWallet getBalance
        unless (balance > 0) . replicateM_ 100 $ generatorGenerate 0

    generatorGenerate nSeries = do
        addr <- RPC.withWallet miningWallet newAddress
        blockHash <- RPC.generateToAddress 1 addr Nothing
        balance <- RPC.withWallet miningWallet getBalance
        when (balance >= satRequirement && nSeries <= maxSeries) fundFloodWallet
        pure blockHash

    fundFloodWallet = do
        addressAmounts <-
            RPC.withWallet floodingWallet $
                traverse getAddressAmount outputsPerSeries
        feeRate <- getMeanFeeRate <$> RPC.getBlockCount
        void . RPC.withWallet miningWallet $ spend feeRate addressAmounts

    getAddressAmount nOutputs = (,amountPerOutput * nOutputs) <$> newAddress

    newAddress = RPC.getNewAddress Nothing Nothing

    fundAddress amount addr = do
        feeRate <- getMeanFeeRate <$> RPC.getBlockCount
        spend feeRate [(addr, amount)]

    handlePayout fundingAddress fundingAmount = fix $ \retry -> do
        balance <- getBalance
        if balance >= fundingAmount
            then void $ fundAddress fundingAmount fundingAddress
            else liftIO (threadDelay 1_000_000) >> retry

satRequirement :: Word64
satRequirement = sum $ (* amountPerOutput) <$> outputsPerSeries

{- | Create many transactions and fill up blockspace.  This uses the same trick
 as the Bitcoin Core functional tests: create anyone can spend outputs with
 predictable outputs to avoid needing to sign.

 We create sequences of transactions which start by breaking a UTXO into some number of outputs.
 Subsequent transactions spend all the outputs of the previous transaction and create the same
 number of outputs.
-}
txFlood ::
    -- | Fee rate per block
    (BlockHeight -> Word64) ->
    StateT [TxSeries] BitcoindClient [TxHash]
txFlood feeRate = do
    balance <- lift getBalance
    nSeries <- getSeriesCount
    when (balance >= satRequirement && nSeries <= maxSeries) $ do
        height <- lift RPC.getBlockCount
        -- Create a series out of every unspent output
        unspentOutputs <- lift getUnspent
        -- We use coin control to make sure that the wallet spends outputs
        -- in a predictable way
        lift . RPC.lockUnspent False $ fst <$> unspentOutputs
        mapM_ (createSeries height) unspentOutputs
    flood
  where
    createSeries height (outPoint, amount) = do
        lift $ RPC.lockUnspent True [outPoint]
        fund (fromIntegral nOutputs) (feeRate height) amount
      where
        nOutputs = amount `quot` amountPerOutput

    getUnspent =
        fmap extractPointAmount
            <$> RPC.listUnspent
                Nothing
                Nothing
                Nothing
                Nothing
                (ListUnspentOptions Nothing Nothing Nothing Nothing)
    extractPointAmount =
        (OutPoint <$> RPC.outputTxId <*> RPC.outputVOut) &&& RPC.outputAmount

getBalance :: BitcoindClient Word64
getBalance = RPC.balanceDetailsTrusted . RPC.balancesMine <$> RPC.getBalances

getSeriesCount :: Monad m => StateT [TxSeries] m Int
getSeriesCount = gets length

type TxSeries = [Tx]

-- | Create the funding anchor for a tx series and add the tx series to the internal state
fund ::
    -- | Output count per transaction
    Int ->
    -- | Fee rate
    Word64 ->
    -- | Sats to allocate
    Word64 ->
    StateT [TxSeries] BitcoindClient TxHash
fund nOutputs feeRate sats = do
    (value, outPoint) <- lift createFundingOutput
    modify' $ (:) (take seriesLength $ txSeries nOutputs feePerOutput value outPoint)
    pure $ H.outPointHash outPoint
  where
    createFundingOutput = do
        tx <- createRoot >>= (`RPC.getRawTransaction` Nothing)
        pure
            ( H.outValue . head $ H.txOut tx
            , OutPoint (H.txHash tx) 0
            )

    createRoot =
        RPC.sendToAddress
            easySpendAddress
            sats
            Nothing
            Nothing
            (Just True)
            Nothing
            Nothing
            Nothing
            Nothing
            (Just feeRate)

    feePerOutput = feeRate * 75 -- tuned via testing

-- | Peel off the next transaction from each series and broadcast it
flood :: StateT [TxSeries] BitcoindClient [TxHash]
flood = StateT (pure . advance) >>= lift . traverse (`RPC.sendTransaction` Nothing)

advance :: [TxSeries] -> ([Tx], [TxSeries])
advance = peelFirst &&& dropFirst
  where
    peelFirst = mapMaybe (fmap fst . uncons)
    dropFirst = filter (not . null) . fmap (drop 1)

{- | Construct a sequence of transactions each of which spends all of the inputs of the previous
 one, creating the same number of outputs
-}
txSeries ::
    -- | Number of outputs
    Int ->
    -- | Fee per output
    Word64 ->
    -- | Funds for the series
    Word64 ->
    OutPoint ->
    TxSeries
txSeries nOutputs feePerOutput totalSats fundingPoint =
    unfoldr (sequence . (id &&& getNext)) tx0
  where
    tx0 =
        Tx
            { H.txVersion = 2
            , H.txIn = [spendEasy fundingPoint]
            , H.txOut = replicate nOutputs $ easySpendOutput satsPerOutput0
            , H.txWitness = mempty
            , H.txLockTime = 0
            }
    satsPerOutput0 = (totalSats `quot` fromIntegral nOutputs) - feePerOutput

    getNext prevTx
        | satsPerOutput > 2 * feePerOutput =
            Just
                Tx
                    { H.txVersion = 2
                    , H.txIn = spendEasy <$> outPoints prevTx
                    , H.txOut = replicate nOutputs $ easySpendOutput satsPerOutput
                    , H.txWitness = mempty
                    , H.txLockTime = 0
                    }
        | otherwise = Nothing
      where
        satsPerOutput = (H.outValue . head . H.txOut) prevTx - feePerOutput

    outPoints tx = OutPoint (H.txHash tx) <$> [0 .. (fromIntegral . length . H.txOut) tx - 1]

spend ::
    Word64 ->
    [(Text, Word64)] ->
    BitcoindClient TxHash
spend feeRate addrAmounts =
    RPC.sendMany
        (Map.fromList addrAmounts)
        Nothing
        mempty
        Nothing
        Nothing
        Nothing
        (Just feeRate)

data BlockDelta = BlockDelta
    { blockSpends :: Set OutPoint
    , blockCreates :: Set (OutPoint, Word64)
    }

{- | Identify the outputs flooded by a previous run that are unspent and sweep
some of them into the mining wallet.  This makes it possible to avoid starting
out by mining 100 blocks.
-}
sweep ::
    -- | Fee per input
    Word64 ->
    BitcoindClient Int
sweep feePerInput = do
    currentHeight <- RPC.getBlockCount
    sweepableOutputs <- foldM onBlockHeight mempty [0 .. currentHeight]
    mapM_ onOutputCluster
        . take 500
        . groupsOf 100
        . Set.toList
        $ sweepableOutputs
    pure $ Set.size sweepableOutputs
  where
    onBlockHeight !utxos n = updateUtxoSet utxos <$> getSpentUnspent n
    getSpentUnspent = RPC.getBlockHash >=> fmap onBlock . RPC.getBlock
    onBlock = finalizeBlockDelta . foldl' onTransaction (BlockDelta mempty mempty) . H.blockTxns
    onTransaction delta tx =
        BlockDelta
            { blockSpends = blockSpends delta <> Set.fromList (H.prevOutput <$> H.txIn tx)
            , blockCreates =
                blockCreates delta
                    <> ( Set.fromList
                            . fmap (outPointValue (H.txHash tx))
                            . filter (isEasySpend . snd)
                            . zip [0 ..]
                            . H.txOut
                       )
                        tx
            }
    outPointValue txHash (ix, txOut) = (OutPoint txHash ix, H.outValue txOut)
    isEasySpend txOut = H.scriptOutput txOut == easySpendScriptOutput
    finalizeBlockDelta delta =
        BlockDelta
            { blockSpends = blockSpends delta
            , blockCreates = Set.filter (isUnspent delta) $ blockCreates delta
            }

    updateUtxoSet utxos delta = Set.filter (isUnspent delta) utxos <> blockCreates delta
    isUnspent delta (outPoint, _) = not . Set.member outPoint $ blockSpends delta

    onOutputCluster outputs = do
        scriptOutput <-
            RPC.getNewAddress Nothing Nothing
                >>= maybe
                    (error "Unable to decode address")
                    (pure . H.addressToScriptBS)
                    . H.textToAddr H.btcRegTest
        RPC.sendTransaction
            ( Tx
                { txVersion = 2
                , txIn = spendEasy . fst <$> outputs
                , txOut =
                    [ H.TxOut
                        { H.outValue = sum (subtract feePerInput . snd <$> outputs)
                        , H.scriptOutput
                        }
                    ]
                , txWitness = mempty
                , txLockTime = 0
                }
            )
            Nothing

    groupsOf n = \case
        [] -> []
        xs -> uncurry (:) $ groupsOf n <$> splitAt n xs

easySpendAddress :: Text
Just easySpendAddress =
    H.addrToText H.btcRegTest =<< (H.outputAddress . H.toP2SH) (Script mempty)

easySpendScriptOutput :: ByteString
easySpendScriptOutput = H.encodeOutputBS . H.toP2SH $ Script mempty

-- | Produce an output that we can spend without signing
easySpendOutput :: Word64 -> TxOut
easySpendOutput outValue = TxOut{H.outValue, H.scriptOutput = easySpendScriptOutput}

-- | Produce an input that spends an output created with 'easySpendOutput'
spendEasy :: OutPoint -> TxIn
spendEasy prevOutput =
    TxIn
        { H.prevOutput
        , H.scriptInput = S.encode $ Script [H.OP_1, H.opPushData (S.encode $ Script mempty)]
        , H.txInSequence = maxBound
        }

waitRepeat :: MonadIO m => Int -> m a -> m ()
waitRepeat delayInSecs task =
    forever $ task >> (liftIO . threadDelay) (delayInSecs * 1_000_000)
