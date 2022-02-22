{-# LANGUAGE NumericUnderscores #-}

module Bitcoin.Core.Test.Generator (
    testGenerator,
) where

import Bitcoin.Core.RPC (BitcoindClient)
import qualified Bitcoin.Core.RPC as RPC
import Bitcoin.Core.Regtest (
    generateWithTransactions,
    runBitcoind,
    withBitcoind,
 )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, link)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Haskoin (BlockHeight)
import Network.HTTP.Client (Manager)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps, (@?=))

testGenerator :: Manager -> TestTree
testGenerator mgr = testCaseSteps "generateWithTransactions" $ \step ->
    withBitcoind 8459 $ \nodeHandle -> do
        step "Generate some blocks"
        h <- async $ generateWithTransactions mgr nodeHandle 1 (pure Nothing) (const 20)
        link h

        runBitcoind mgr nodeHandle $ waitForBlocks 120
        cancel h

        Right feeResponse <-
            runBitcoind mgr nodeHandle $
                RPC.estimateSmartFeeFee <$> RPC.estimateSmartFee 6 Nothing
        fmap (> 18) feeResponse @?= Just True
        fmap (< 23) feeResponse @?= Just True

waitForBlocks :: BlockHeight -> BitcoindClient ()
waitForBlocks targetHeight = loop
  where
    loop = do
        n <- RPC.getBlockCount
        unless (n >= targetHeight) $ liftIO (threadDelay 1_000_000) >> loop
