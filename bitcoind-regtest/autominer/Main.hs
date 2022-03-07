{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bitcoin.Core.RPC (
    BitcoindClient,
    Command (Add),
    addNode,
    getPeerInfo,
    startingHeight,
    syncedBlocks,
 )
import Bitcoin.Core.Regtest (
    Funding (OneTime),
    generateWithTransactions,
    nodeP2pPort,
    runBitcoind,
    withBitcoind,
 )
import Control.Applicative (many)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forM_, (<=<))
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
    , initialFunding :: Funding
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
            <*> (OneTime <$> many optFunding)

    optBasePort =
        Opt.option Opt.auto $
            Opt.long "basePort"
                <> Opt.value 18044
                <> Opt.help "The first in the sequence of ports the miner will use"
                <> Opt.showDefault

    optBlockInterval =
        Opt.option Opt.auto $
            Opt.long "blockInterval"
                <> Opt.value 60
                <> Opt.showDefault

    optPeerPort =
        Opt.option Opt.auto $
            Opt.long "peerPort"
                <> Opt.help "Connect to a peer at this port"

    optFunding = (,) <$> optFundingAddress <*> optFundingAmount

    optFundingAddress =
        Opt.strOption $
            Opt.long "fundingAddress"
                <> Opt.help "Use this address to collect funds"

    optFundingAmount =
        Opt.option Opt.auto $
            Opt.long "fundingAmount"
                <> Opt.help "The amount of satoshis with which to fund"
                <> Opt.value 100_000_000
                <> Opt.showDefault

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    config <- Opt.execParser options
    withBitcoind (basePort config) $ \nodeHandle -> do
        putStrLn $ "Listening for peers on 127.0.0.1:" <> show (nodeP2pPort nodeHandle)
        forM_ (show <$> peerPort config) $ \thePeerPort -> do
            putStrLn $ "Connecting to a peer on port " <> thePeerPort
            either throwIO pure <=< runBitcoind mgr nodeHandle $ do
                addNode ("127.0.0.1:" <> pack thePeerPort) Add
                waitSync

        putStrLn "Generating blocks..."
        generateWithTransactions
            mgr
            nodeHandle
            (blockInterval config)
            (Just $ initialFunding config)
            oscillatingFeeRate

oscillatingFeeRate :: BlockHeight -> Word64
oscillatingFeeRate n
    | even (n `quot` 100) = 25
    | otherwise = 3

waitSync :: BitcoindClient ()
waitSync = do
    getPeerInfo >>= maybe retry onPeerInfo . listToMaybe
  where
    onPeerInfo peerInfo
        | Just synced <- syncedBlocks peerInfo
        , synced < startingHeight peerInfo =
            retry
        | otherwise = pure ()
    retry = do
        liftIO $ putStrLn "Waiting for sync"
        liftIO $ threadDelay 1_000_000
        waitSync
