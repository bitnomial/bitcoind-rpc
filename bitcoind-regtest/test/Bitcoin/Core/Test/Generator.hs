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
import Control.Monad (forM_, unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Haskoin (Block, BlockHeight, blockTxns)
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

        forM_ [102 .. 120] $ \blockHeight -> do
            Right block <- runBitcoind mgr nodeHandle $ getBlockAtHeight blockHeight
            length (blockTxns block) @?= 22

        feeResponse <-
            runBitcoind mgr nodeHandle $
                RPC.estimateSmartFeeFee <$> RPC.estimateSmartFee 6 Nothing
        first show feeResponse @?= Right (Just 20)

getBlockAtHeight :: BlockHeight -> BitcoindClient Block
getBlockAtHeight = RPC.getBlockHash >=> RPC.getBlock

waitForBlocks :: BlockHeight -> BitcoindClient ()
waitForBlocks targetHeight = loop
  where
    loop = do
        n <- RPC.getBlockCount
        unless (n >= targetHeight) $ liftIO (threadDelay 1_000_000) >> loop
