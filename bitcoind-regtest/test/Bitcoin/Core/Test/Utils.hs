{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Core.Test.Utils (
    testRpc,
    bitcoindTest,
    shouldMatch,

    -- * Wallet
    createWallet,
    initWallet,
    generate,
    toInput,
) where

import Data.Functor (void)
import Network.HTTP.Client (Manager)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Bitcoin.Core.RPC (BitcoindClient, LoadWalletResponse, OutputDetails, PsbtInput (PsbtInput))
import qualified Bitcoin.Core.RPC as RPC
import Bitcoin.Core.Regtest (NodeHandle)
import qualified Bitcoin.Core.Regtest as R
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

testRpc :: String -> BitcoindClient r -> (String, BitcoindClient ())
testRpc name x = (name, void x)

bitcoindTest :: Manager -> NodeHandle -> (String, BitcoindClient ()) -> TestTree
bitcoindTest mgr h (name, task) =
    testCase name $ R.runBitcoind mgr h task >>= either onFail pure
  where
    onFail = assertFailure . show

shouldMatch :: (Eq a, Show a) => a -> a -> BitcoindClient ()
shouldMatch expected seen = liftIO $ seen @?= expected

createWallet :: Text -> BitcoindClient LoadWalletResponse
createWallet walletName =
    RPC.createWallet
        walletName
        Nothing
        Nothing
        mempty
        (Just True)
        Nothing
        Nothing
        Nothing

initWallet :: Text -> BitcoindClient ()
initWallet name = void $ createWallet name

generate :: BitcoindClient ()
generate = do
    addr <- RPC.getNewAddress Nothing Nothing
    void $ RPC.generateToAddress 1 addr Nothing

toInput :: OutputDetails -> PsbtInput
toInput = PsbtInput <$> RPC.outputTxId <*> RPC.outputVOut <*> pure Nothing
