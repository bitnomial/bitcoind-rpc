{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Core.Regtest.Generator (
    generateWithTransactions,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever, replicateM)
import qualified Data.Serialize as S
import Data.Word (Word64)
import Haskoin.Block (BlockHeight, blockTxns)
import Haskoin.Transaction (
    OutPoint (..),
    Tx (..),
    TxOut (..),
    txHash,
 )
import Network.HTTP.Client (Manager)

import qualified Bitcoin.Core.RPC as RPC
import Bitcoin.Core.Regtest.Framework (NodeHandle, runBitcoind)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (evalStateT, get, gets, modify, put)
import Data.ByteString.Base64 (encodeBase64)
import Data.Maybe (fromJust)
import Data.Sequence (Seq ((:<|)), (|>))

processCoinbase :: Tx -> (OutPoint, Word64)
processCoinbase tx0 = (OutPoint (txHash tx0) 0, outValue . head $ txOut tx0)

data GeneratorState = GeneratorState
    { generatorSpendableOutputs :: [(OutPoint, Word64)]
    , generatorUnspentCoinbases :: Seq (OutPoint, Word64)
    }

{- | Generate many transactions per block with a certain mean fee rate

FIXME For some reason @bitcoind@ refuses to sign transactions if we set
 @splitCount@ too high.  What's going on here?
-}
generateWithTransactions ::
    Manager ->
    NodeHandle ->
    -- | Block interval (seconds per block)
    Int ->
    -- | Mean fee rate at height
    (BlockHeight -> Word64) ->
    IO ()
generateWithTransactions mgr nodeHandle blockInterval getMeanFeeRate =
    either throwIO pure =<< runBitcoind mgr nodeHandle generator
  where
    generator = flip evalStateT emptyState $ do
        initialize
        forever $ do
            sendTransactions
            generatorGenerate
            liftIO $ threadDelay (blockInterval * 1_000_000)

    emptyState = GeneratorState mempty mempty

    initialize = do
        lift $
            RPC.createWallet
                "bitcoin-regtest-generator"
                Nothing
                Nothing
                password
                Nothing
                Nothing
                Nothing
                Nothing
        replicateM 100 generatorGenerate

    generatorGenerate = do
        coinbaseOutput <- lift $ do
            addr <- newAddress
            RPC.generateToAddress 1 addr Nothing
                >>= fmap (processCoinbase . head . blockTxns)
                    . RPC.getBlock
                    . head
        modify $ \state ->
            state
                { generatorUnspentCoinbases =
                    generatorUnspentCoinbases state |> coinbaseOutput
                }

    newAddress = RPC.getNewAddress Nothing Nothing

    sendTransactions = do
        lift $ RPC.walletPassphrase password (2 * blockInterval)
        feeRate <- getMeanFeeRate <$> lift RPC.getBlockCount
        splitOutputTxs <- spendSplitOutputs feeRate
        coinbaseSplitTx <- popCoinbase >>= traverse (splitCoinbaseOutput feeRate)
        lift . mapM_ (`RPC.sendTransaction` Nothing) $ splitOutputTxs
        lift . mapM_ (`RPC.sendTransaction` Nothing) $ coinbaseSplitTx

    popCoinbase = do
        state <- get
        case generatorUnspentCoinbases state of
            next :<| rest -> Just next <$ put state{generatorUnspentCoinbases = rest}
            _ -> pure Nothing

    appendSpendable tx = modify $ \state ->
        state
            { generatorSpendableOutputs =
                generatorSpendableOutputs state
                    <> zipWith
                        (mkSpendableOutput (txHash tx))
                        [0 ..]
                        (outValue <$> txOut tx)
            }
    mkSpendableOutput txId ix amount = (OutPoint txId ix, amount)

    splitCoinbaseOutput feeRate (coinbaseOutput, amount) = do
        recipients <- lift $ replicateM splitCount newAddress
        let outputs = zip recipients amounts
        tx <- lift $ spendOutput feeRate coinbaseOutput outputs
        appendSpendable tx
        pure tx
      where
        (q, r) = amount `quotRem` splitCount
        amounts = (q +) <$> (replicate (fromIntegral r) 1 <> repeat 0)
        splitCount :: Num a => a
        splitCount = 20

    useSpendable = do
        spendableOutputs <- gets generatorSpendableOutputs
        modify $ \state -> state{generatorSpendableOutputs = mempty}
        pure spendableOutputs

    spendSplitOutputs feeRate = useSpendable >>= lift . traverse (spendSplitOutput feeRate)
    spendSplitOutput feeRate (outPoint, amount) = do
        recipient <- newAddress
        spendOutput feeRate outPoint [(recipient, amount)]

    password = "password"

    spendOutput feeRate inputOutPoint outputs = do
        psbt0 <-
            RPC.createPsbtPsbt
                <$> RPC.createFundedPsbt [psbtInput] psbtOutputs Nothing (Just options) Nothing
        psbt1 <-
            RPC.processPsbtPsbt
                <$> RPC.processPsbt (psbtText psbt0) (Just True) Nothing Nothing
        finalizePsbtResponse <- RPC.finalizePsbt (psbtText psbt1) (Just True)
        pure . fromJust $ RPC.finalizedTx finalizePsbtResponse
      where
        psbtInput =
            RPC.PsbtInput
                { RPC.psbtInputTx = outPointHash inputOutPoint
                , RPC.psbtInputVOut = outPointIndex inputOutPoint
                , RPC.psbtInputSequence = Nothing
                }
        psbtOutputs =
            RPC.PsbtOutputs
                { RPC.psbtOutputAddrs = outputs
                , RPC.psbtOutputData = Nothing
                }
        options =
            RPC.CreatePsbtOptions
                { RPC.createPsbtAddInputs = Just False
                , RPC.createPsbtChangeAddress = Nothing
                , RPC.createPsbtChangePosition = Nothing
                , RPC.createPsbtChangeType = Nothing
                , RPC.createPsbtIncludeWatching = Nothing
                , RPC.createPsbtLockUnspents = Nothing
                , RPC.createPsbtFeeRate = Just feeRate
                , RPC.createPsbtSubtractFee = [0 .. length outputs - 1]
                , RPC.createPsbtReplaceable = Nothing
                , RPC.createPsbtConfTarget = Nothing
                , RPC.createPsbtEstimateMode = Nothing
                }

    psbtText = encodeBase64 . S.encode
