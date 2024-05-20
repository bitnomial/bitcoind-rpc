module Bitcoin.Core.Test.Utils (
    testRpc,
    bitcoindTest,
    shouldMatch,

    -- * Wallet
    createWallet,
    initWallet,
    generate,
    toInput,

    -- * Crypto
    globalContext,
) where

import Bitcoin.Core.RPC (BitcoindClient, LoadWalletResponse, OutputDetails, PsbtInput (PsbtInput))
import qualified Bitcoin.Core.RPC as RPC
import Bitcoin.Core.Regtest (NodeHandle)
import qualified Bitcoin.Core.Regtest as R
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Text (Text)
import Haskoin.Crypto (Ctx, createContext)
import Network.HTTP.Client (Manager)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

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
        (Just False)
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

{- | The global context is created once and never modified again, it is to be passed into cryptographic
functions and contains a number of large data structures that are generated at runtime. Impure functions like
`destroyContext` or `randomizeContext` must not be used against this global value
-}
{-# NOINLINE globalContext #-}
globalContext :: Ctx
globalContext = unsafePerformIO createContext
