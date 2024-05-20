{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Core.Test.PSBT (
    psbtRPC,
) where

import Bitcoin.Core.RPC (
    BitcoindClient,
    Descriptor (Descriptor),
    ListUnspentOptions (ListUnspentOptions),
    PsbtInput,
    PsbtOutputs (PsbtOutputs),
    withWallet,
 )
import qualified Bitcoin.Core.RPC as RPC
import Bitcoin.Core.Regtest (NodeHandle, Version, nodeVersion, v21_0)
import Bitcoin.Core.Test.Utils (
    bitcoindTest,
    generate,
    globalContext,
    initWallet,
    testRpc,
    toInput,
 )
import Control.Monad (replicateM_, when)
import Data.ByteString.Base64 (encodeBase64)
import Data.Functor (void)
import Data.Maybe (mapMaybe)
import qualified Data.Serialize as S
import Haskoin.Transaction (putPSBT)
import Network.HTTP.Client (Manager)
import Test.Tasty (TestTree, testGroup)

psbtRPC :: Manager -> NodeHandle -> TestTree
psbtRPC mgr h =
    testGroup
        "psbt-rpc"
        [bitcoindTest mgr h $ testRpc "psbt" (testPSBT v)]
  where
    v = nodeVersion h

data Fixture = Fixture
    { inputsA :: [PsbtInput]
    , descriptorsA :: [Descriptor]
    , outputsA :: PsbtOutputs
    , inputsB :: [PsbtInput]
    , outputsB :: PsbtOutputs
    }

testPSBT :: Version -> BitcoindClient ()
testPSBT v = when (v >= v21_0) $ do
    initWallet wallet
    fixture <- withWallet wallet $ do
        replicateM_ 200 generate

        utxos <-
            filter ((> 6) . RPC.outputConfs)
                <$> RPC.listUnspent
                    Nothing
                    Nothing
                    Nothing
                    (Just True)
                    (ListUnspentOptions Nothing Nothing Nothing Nothing)

        outputAddrA <- RPC.getNewAddress Nothing Nothing

        let utxosA = take 3 utxos
            utxosB = take 2 $ drop 3 utxos
        pure
            Fixture
                { inputsA = toInput <$> utxosA
                , outputsA = PsbtOutputs [(outputAddrA, 1_0000_0000)] Nothing
                , descriptorsA = Descriptor <$> mapMaybe RPC.outputDescriptor utxosA
                , inputsB = toInput <$> utxosB
                , outputsB = PsbtOutputs mempty (Just "aabbcc")
                }
    psbtA <-
        toBase64
            <$> RPC.createPsbt
                (inputsA fixture)
                (outputsA fixture)
                (Just 5)
                (Just True)

    RPC.analyzePsbt psbtA

    psbtB <-
        toBase64
            <$> RPC.createPsbt
                (inputsB fixture)
                (outputsB fixture)
                (Just 5)
                (Just True)

    RPC.utxoUpdatePsbt psbtA $ descriptorsA fixture

    psbtC <- toBase64 <$> RPC.joinPsbts [psbtA, psbtB]
    RPC.finalizePsbt psbtC Nothing

    void . withWallet wallet $
        RPC.processPsbt
            psbtC
            (Just True)
            Nothing
            (Just True)
  where
    wallet = "testPSBT"

    toBase64 = encodeBase64 . S.runPut . putPSBT globalContext
