{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Bitcoin.Core.Regtest.Generator (
    GeneratorConfig (..),
    GeneratorStatus (..),
    GeneratorState (..),
    GeneratorHandle (..),
    generateWithTransactions,
) where

import Bitcoin.Core.RPC (
    BitcoindClient,
 )
import qualified Bitcoin.Core.RPC as RPC
import Bitcoin.Core.Regtest.Framework (NodeHandle, runBitcoind)
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, link)
import Control.Exception (throwIO)
import Control.Monad (foldM, replicateM_, unless, when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (
    StateT,
    evalStateT,
    gets,
    modify',
    state,
 )
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn, uncons, unfoldr)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Serialize as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64, Word8)
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
amountPerOutput = 20_000

-- | This value determines the transaction series that will fill up the mempool
outputsPerSeries :: [Word64]
outputsPerSeries =
    mconcat
        [ replicate 12 100 -- 12 txs with 100 outputs
        , replicate 18 5 -- 18 txs with 5 outputs
        , replicate 60 2 -- 60 txs with 2 outputs
        ]

data GeneratorConfig = GeneratorConfig
    { blockInterval :: Int
    -- ^ Block interval (seconds per block)
    , getMeanFeeRate :: BlockHeight -> Word64
    -- ^ Mean fee rate at height
    }

data GeneratorStatus
    = Starting
    | Running
    | Paused
    | Stopped
    deriving (Eq, Show)

data GeneratorState = GeneratorState
    { status :: GeneratorStatus
    , balance :: Word64
    , blocksMined :: Int
    }
    deriving (Eq, Show)

data GeneratorHandle = GeneratorHandle
    { getGeneratorState :: IO GeneratorState
    , pauseGenerator :: IO ()
    , resumeGenerator :: IO ()
    , stopGenerator :: IO ()
    , makePayment :: [(Text, Word64)] -> IO ()
    , reconfigure :: GeneratorConfig -> IO ()
    , generatorAsync :: Async ()
    -- ^ Use to wait on the generator process
    }

-- | Generate many transactions per block with a certain mean fee rate
generateWithTransactions ::
    Manager ->
    NodeHandle ->
    GeneratorConfig ->
    IO GeneratorHandle
generateWithTransactions mgr nodeHandle initGeneratorConfig = do
    refState <-
        newIORef
            GeneratorState
                { status = Starting
                , balance = 0
                , blocksMined = 0
                }
    refConfig <- newIORef initGeneratorConfig
    refPaymentQueue <- newIORef mempty
    refStatusRequest <- newIORef Nothing

    let getStatus = status <$> readIORef refState
        setStatus status =
            atomicModifyIORef' refState $
                \genState -> (genState{status}, status)
        requestStatus =
            atomicModifyIORef' refStatusRequest . updateRef . const . Just
        onStatusRequest theStatus = do
            -- Clear the existing status request
            atomicModifyIORef' refStatusRequest . updateRef $ const Nothing
            setStatus theStatus
        addBlocks n = atomicModifyIORef' refState . updateRef $ \genState ->
            genState
                { blocksMined = n + blocksMined genState
                }
        handlePayments config balance = do
            feeRate <- getMeanFeeRate config <$> lift RPC.getBlockCount
            payments <-
                (liftIO . atomicModifyIORef' refPaymentQueue)
                    (first reverse . popPayments balance . reverse)
            unless (null payments)
                . void
                . lift
                . RPC.withWallet miningWallet
                -- Use a higher feerate to jump the line
                $ spend (feeRate + 10) payments
        mainLoop = do
            liftIO
                (readIORef refStatusRequest)
                >>= liftIO . maybe getStatus onStatusRequest
                >>= \case
                    -- We are done
                    Stopped -> pure ()
                    -- Poll rather than doing a round
                    Paused -> do
                        config <- liftIO $ readIORef refConfig
                        balance <- lift $ RPC.withWallet miningWallet getBalance
                        handlePayments config balance
                        liftIO (threadDelay oneSec)
                        mainLoop
                    _ -> do
                        config <- liftIO $ readIORef refConfig
                        txFlood $ getMeanFeeRate config
                        getSeriesCount
                            >>= fmap snd . lift . generatorGenerate config
                            >>= mapM_ pushRoots
                        liftIO $ addBlocks 1
                        -- Update the balance
                        balance <- lift $ RPC.withWallet miningWallet getBalance
                        liftIO
                            . atomicModifyIORef' refState
                            . updateRef
                            $ \genState -> genState{balance}
                        -- Payments
                        handlePayments config balance
                        -- Delay until next round
                        liftIO . threadDelay $ blockInterval config * oneSec
                        mainLoop

    generatorAsync <- async $ do
        (initFloodState, initBlocks) <- run initialize
        addBlocks initBlocks
        setStatus Running
        run $ evalStateT mainLoop initFloodState
    link generatorAsync
    pure
        GeneratorHandle
            { getGeneratorState = readIORef refState
            , pauseGenerator = requestStatus Paused
            , resumeGenerator = requestStatus Running
            , stopGenerator = requestStatus Stopped
            , makePayment = atomicModifyIORef' refPaymentQueue . updateRef . (<>)
            , reconfigure = atomicModifyIORef' refConfig . updateRef . const
            , generatorAsync
            }
  where
    run = runBitcoind mgr nodeHandle >=> either throwIO pure

    miningWallet = "bitcoin-regtest-generator.mine"

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
        floodRoots <- sweep
        let balance = sum $ floodAmount <$> floodRoots
            blocksToMine = if balance > 0 then 0 else 100
        replicateM_ blocksToMine $ generatorGenerate initGeneratorConfig 0
        pure (FloodState{floodRoots, floodSeries = mempty}, blocksToMine)

    generatorGenerate config nSeries = do
        addr <- RPC.withWallet miningWallet newAddress
        balance <- RPC.withWallet miningWallet getBalance
        fundRootTx <-
            if balance >= satRequirement && nSeries <= maxSeries
                then Just <$> fundFloodWallet config
                else pure Nothing
        (,fundRootTx) <$> RPC.generateToAddress 1 addr Nothing

    fundFloodWallet config = do
        let addressAmounts =
                uncurry getAddressAmount <$> zip [0 ..] outputsPerSeries
        feeRate <- getMeanFeeRate config <$> RPC.getBlockCount
        RPC.withWallet miningWallet (spend feeRate addressAmounts)
            >>= (`RPC.getRawTransaction` Nothing)

    pushRoots tx =
        mapM_ pushFloodRoot
            . zipMerge
                (getOutPointAmount $ H.txHash tx)
                snd
                (H.scriptOutput . snd)
                batchAddresses
            . sortOn (H.scriptOutput . snd)
            . zip [0 ..]
            $ H.txOut tx

    batchAddresses =
        sortOn snd $
            (id &&& easySpendScriptOutput . Just)
                <$> [0 ..]

    getOutPointAmount txh (ix, _) (vout, txOut) =
        FloodRootPoint
            { floodOutPoint = OutPoint txh vout
            , floodOutPointLabel = Just ix
            , floodAmount = H.outValue txOut
            , floodSwept = False
            }

    getAddressAmount ix nOutputs =
        ( easySpendAddress $ Just ix
        , amountPerOutput * nOutputs
        )

    newAddress = RPC.getNewAddress Nothing Nothing

popPayments ::
    -- | Budget
    Word64 ->
    -- | Payment queue
    [(Text, Word64)] ->
    -- | (remaining payments, feasible payments)
    ([(Text, Word64)], [(Text, Word64)])
popPayments = accum mempty
  where
    accum payable remainingBalance = \case
        x@(addr, amount) : xs
            | amount <= remainingBalance
            , addr `notElem` (fst <$> payable) ->
                accum (x : payable) (remainingBalance - amount) xs
        payments -> (payments, payable)

oneSec :: Int
oneSec = 1_000_000

satRequirement :: Word64
satRequirement = sum $ (* amountPerOutput) <$> outputsPerSeries

data FloodRootPoint = FloodRootPoint
    { floodOutPoint :: OutPoint
    , floodOutPointLabel :: Maybe Word8
    , floodAmount :: Word64
    , floodSwept :: Bool
    }
    deriving (Eq, Show)

data FloodState = FloodState
    { floodRoots :: [FloodRootPoint]
    , floodSeries :: [TxSeries]
    }
    deriving (Eq, Show)

pushFloodRoot :: (Monad m) => FloodRootPoint -> StateT FloodState m ()
pushFloodRoot rootPoint = modify' $ \floodState ->
    floodState{floodRoots = rootPoint : floodRoots floodState}

flushFloodRoots :: (Monad m) => Int -> StateT FloodState m [FloodRootPoint]
flushFloodRoots n = do
    (flushedRoots, remainingRoots) <- splitAt n <$> gets floodRoots
    modify' $ \floodState -> floodState{floodRoots = remainingRoots}
    pure flushedRoots

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
    StateT FloodState BitcoindClient [TxHash]
txFlood feeRate = do
    balance <- getFloodBalance
    nSeries <- getSeriesCount
    when (balance >= satRequirement && nSeries <= maxSeries) $ do
        height <- lift RPC.getBlockCount
        -- Create a series out of every unspent output
        mapM_ (createSeries height) =<< flushFloodRoots maxSeries
    flood
  where
    createSeries height floodRootPoint =
        buildSeries (fromIntegral nOutputs) (feeRate height) floodRootPoint
      where
        nOutputs = max 1 $ floodAmount floodRootPoint `quot` amountPerOutput

getFloodBalance :: StateT FloodState BitcoindClient Word64
getFloodBalance = gets $ sum . fmap floodAmount . floodRoots

getBalance :: BitcoindClient Word64
getBalance = RPC.balanceDetailsTrusted . RPC.balancesMine <$> RPC.getBalances

getSeriesCount :: (Monad m) => StateT FloodState m Int
getSeriesCount = gets $ length . floodSeries

type TxSeries = [Tx]

-- | Create the funding anchor for a tx series and add the tx series to the internal state
buildSeries ::
    -- | Output count per transaction
    Int ->
    -- | Fee rate
    Word64 ->
    FloodRootPoint ->
    StateT FloodState BitcoindClient ()
buildSeries nOutputs feeRate floodRootPoint =
    modify' . updateSeries $
        (:) (take seriesLength $ txSeries nOutputs feePerOutput floodRootPoint)
  where
    feePerOutput = feeRate * 75 -- tuned via testing
    updateSeries f floodState = floodState{floodSeries = f $ floodSeries floodState}

-- | Peel off the next transaction from each series and broadcast it
flood :: StateT FloodState BitcoindClient [TxHash]
flood = advance >>= lift . traverse (`RPC.sendTransaction` Nothing)

advance :: (Monad m) => StateT FloodState m [Tx]
advance = state . asFloodStateUpdate $ peelFirst &&& dropFirst
  where
    peelFirst = mapMaybe (fmap fst . uncons)
    dropFirst = filter (not . null) . fmap (drop 1)
    asFloodStateUpdate f floodState = (x, floodState{floodSeries = newSeries})
      where
        (x, newSeries) = f $ floodSeries floodState

{- | Construct a sequence of transactions each of which spends all of the inputs of the previous
 one, creating the same number of outputs
-}
txSeries ::
    -- | Number of outputs
    Int ->
    -- | Fee per output
    Word64 ->
    FloodRootPoint ->
    TxSeries
txSeries nOutputs feePerOutput floodRootPoint =
    unfoldr (sequence . (id &&& getNext)) tx0
  where
    tx0 =
        Tx
            { H.txVersion = 2
            , H.txIn =
                [spendEasy <$> floodOutPointLabel <*> floodOutPoint $ floodRootPoint]
            , H.txOut = replicate nOutputs $ easySpendOutput Nothing satsPerOutput0
            , H.txWitness = mempty
            , H.txLockTime = 0
            }
    satsPerOutput0 = (floodAmount floodRootPoint `quot` fromIntegral nOutputs) - feePerOutput

    getNext prevTx
        | satsPerOutput > 2 * feePerOutput =
            Just
                Tx
                    { H.txVersion = 2
                    , H.txIn = spendEasy Nothing <$> outPoints prevTx
                    , H.txOut = replicate nOutputs $ easySpendOutput Nothing satsPerOutput
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
sweep :: BitcoindClient [FloodRootPoint]
sweep = do
    currentHeight <- RPC.getBlockCount
    fmap toFloodRootPoint . Set.toList <$> foldM onBlockHeight mempty [0 .. currentHeight]
  where
    onBlockHeight !utxos n = updateUtxoSet utxos <$> getSpentUnspent n
    getSpentUnspent = RPC.getBlockHash >=> fmap onBlock . getBlock
    getBlock h = RPC.getBlockBlock <$> RPC.getBlock h (Just 0)
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
    finalizeBlockDelta delta =
        BlockDelta
            { blockSpends = blockSpends delta
            , blockCreates = Set.filter (isUnspent delta) $ blockCreates delta
            }

    updateUtxoSet utxos delta = Set.filter (isUnspent delta) utxos <> blockCreates delta
    isUnspent delta (outPoint, _) = not . Set.member outPoint $ blockSpends delta

    toFloodRootPoint (floodOutPoint, floodAmount) =
        FloodRootPoint
            { floodOutPoint
            , floodOutPointLabel = Nothing
            , floodAmount
            , floodSwept = True
            }

easySpendScript :: Maybe Word8 -> Script
easySpendScript = Script . maybe mempty mkScript
  where
    mkScript ix =
        [ pushData ix
        , H.OP_DROP
        ]
    pushData ix
        | ix == 0 = H.OP_0
        | ix <= 16 = H.intToScriptOp (fromIntegral ix)
        | otherwise = H.opPushData $ BS.pack [ix]

easySpendAddress :: Maybe Word8 -> Text
easySpendAddress ix = addr
  where
    Just addr =
        H.addrToText H.btcRegTest =<< (H.outputAddress . H.toP2SH) (easySpendScript ix)

easySpendScriptOutput :: Maybe Word8 -> ByteString
easySpendScriptOutput = H.encodeOutputBS . H.toP2SH . easySpendScript

-- | Produce an output that we can spend without signing
easySpendOutput :: Maybe Word8 -> Word64 -> TxOut
easySpendOutput ix outValue = TxOut{H.outValue, H.scriptOutput = easySpendScriptOutput ix}

-- | This only detects easy spend outputs with an empty script
isEasySpend :: TxOut -> Bool
isEasySpend txOut = H.scriptOutput txOut == easySpendScriptOutput Nothing

-- | Produce an input that spends an output created with 'easySpendOutput'
spendEasy :: Maybe Word8 -> OutPoint -> TxIn
spendEasy ix prevOutput =
    TxIn
        { H.prevOutput
        , H.scriptInput = S.encode $ Script [H.OP_1, H.opPushData (S.encode $ easySpendScript ix)]
        , H.txInSequence = maxBound
        }

{- | Given two lists sorted by the value of the provided keying functions, merge
the equal entries using the combining function.
-}
zipMerge ::
    (Ord k) =>
    (a -> b -> c) ->
    (a -> k) ->
    (b -> k) ->
    [a] ->
    [b] ->
    [c]
zipMerge _ _ _ [] _ = []
zipMerge _ _ _ _ [] = []
zipMerge f pA pB xs@(x : remXs) ys@(y : remYs) = case compare (pA x) (pB y) of
    LT -> go remXs ys
    EQ -> f x y : go remXs remYs
    GT -> go xs remYs
  where
    go = zipMerge f pA pB

updateRef :: (a -> a) -> a -> (a, ())
updateRef f = f &&& const ()
