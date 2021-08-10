{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bitcoin.Core.RPC (
    BitcoindClient,
    Command (Add),
    addNode,
    getBlockCount,
    getPeerInfo,
    syncedBlocks,
 )
import Bitcoin.Core.Regtest (
    generateWithTransactions,
    nodeP2pPort,
    runBitcoind,
    withBitcoind,
 )
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Data.Text (pack)
import Data.Word (Word64)
import Haskoin (BlockHeight)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative (ParserInfo, optional, (<**>))
import qualified Options.Applicative as Opt

data Config = Config
    { basePort :: Int
    , blockInterval :: Int
    , peerPort :: Maybe Int
    }

options :: ParserInfo Config
options = Opt.info (opts <**> Opt.helper) desc
  where
    desc = Opt.progDesc "Automatically generate blocks full of transactions"

    opts =
        Config
            <$> optBasePort
            <*> optBlockInterval
            <*> optional optPeerPort

    optBasePort =
        Opt.option Opt.auto $
            Opt.long "basePort"
                <> Opt.value 18044
                <> Opt.help "default: start at port 18044"

    optBlockInterval =
        Opt.option Opt.auto $
            Opt.long "blockInterval"
                <> Opt.value 60
                <> Opt.help "default: 60 seconds"

    optPeerPort =
        Opt.option Opt.auto $
            Opt.long "peerPort"
                <> Opt.help "Connect to a peer at this address"

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    config <- Opt.execParser options
    withBitcoind (basePort config) $ \nodeHandle -> do
        putStrLn $ "Listening for peers on 127.0.0.1:" <> show (nodeP2pPort nodeHandle)
        forM_ (show <$> peerPort config) $ \thePeerPort -> do
            putStrLn $ "Connecting to a peer on port " <> thePeerPort
            runBitcoind mgr nodeHandle $ do
                addNode ("127.0.0.1:" <> pack thePeerPort) Add
                waitSync

        putStrLn "Generating blocks..."
        generateWithTransactions mgr nodeHandle (blockInterval config) oscillatingFeeRate

oscillatingFeeRate :: BlockHeight -> Word64
oscillatingFeeRate n
    | even (n `quot` 100) = 25
    | otherwise = 3

waitSync :: BitcoindClient ()
waitSync = do
    syncedBlockCount <- fmap syncedBlocks . listToMaybe <$> getPeerInfo
    ourBlockCount <- getBlockCount
    when (Just ourBlockCount /= syncedBlockCount) $ do
        liftIO $ putStrLn "Waiting for sync"
        liftIO $ threadDelay 1_000_000
        waitSync
