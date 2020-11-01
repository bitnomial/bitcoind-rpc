{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM, void, (>=>))
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Client (
    defaultManagerSettings,
    newManager,
 )
import Network.Haskoin.Address (
    Address (..),
    addrToString,
    pubKeyAddr,
 )
import Network.Haskoin.Block (
    Block (..),
    BlockHash,
    BlockHeader,
 )
import Network.Haskoin.Constants (btcTest)
import Network.Haskoin.Crypto (Hash160, SecKey)
import Network.Haskoin.Keys (
    PubKeyI,
    derivePubKeyI,
    secKey,
    wrapSecKey,
 )
import Network.Haskoin.Transaction (
    OutPoint (..),
    Tx (..),
    TxHash,
    txHash,
 )
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (assertFailure, testCase)

import Bitcoin.Core.RPC
import Bitcoin.Core.Regtest (NodeHandle, withBitcoind)
import qualified Bitcoin.Core.Regtest as R

main :: IO ()
main =
    defaultMain . testCase "bitcoind-rpc" . withBitcoind 8449 $
        bitcoindTests
            [ testRpc "generatetoaddress" testGenerate
            , testRpc "getbestblockhash" getBestBlockHash
            , testRpc "getblockhash" $ getBlockHash 1
            , testRpc "getblockheader" testBlockHeader
            , testRpc "getblock" testBlock
            , testRpc "getblockcount" getBlockCount
            , testRpc "getdifficulty" getDifficulty
            , testRpc "getchaintips" getChainTips
            , testRpc "getchaintxstats" $ getChainTxStats Nothing Nothing
            , testRpc "getmempoolinfo" getMempoolInfo
            , testRpc "getrawmempool" getRawMempool
            , testRpc "getrawtransaction" testGetTransaction
            , testRpc "sendrawtransaction" testSendTransaction
            , testRpc "createOutput" testCreateOutput
            , testRpc "getmempoolancestors" testMempoolAncestors
            , testRpc "getmempooldescendants" testMempoolDescendants
            , testRpc "getpeerinfo" getPeerInfo
            , testRpc "getconnectioncount" getConnectionCount
            , testRpc "getnodeaddresses" $ getNodeAddresses (Just 10)
            , testRpc "getaddednodeinfo" $ getAddedNodeInfo Nothing
            , testRpc "listbanned" listBanned
            , testRpc "getnettotals" getNetTotals
            , testRpc "uptime" uptime
            , testRpc "addnode" $ addNode "127.0.0.1:8448" Add
            , testRpc "clearbanned" clearBanned
            ]

testGenerate :: BitcoindClient [BlockHash]
testGenerate = generateToAddress 120 addrText Nothing

key :: SecKey
Just key = secKey "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

pk :: PubKeyI
pk = derivePubKeyI $ wrapSecKey True key

addr :: Address
addrHash :: Hash160
addr@(PubKeyAddress addrHash) = pubKeyAddr pk

addrText :: Text
Just addrText = addrToString btcTest addr

testBlock :: BitcoindClient Block
testBlock = getBestBlockHash >>= getBlock

testBlockHeader :: BitcoindClient BlockHeader
testBlockHeader = getBestBlockHash >>= getBlockHeader

testBlockStats :: BitcoindClient BlockStats
testBlockStats = getBestBlockHash >>= getBlockStats

testGetTransaction :: BitcoindClient Tx
testGetTransaction =
    getBestBlockHash >>= getBlock >>= (`getTransaction` Nothing) . txHash . head . blockTxns

testSendTransaction :: BitcoindClient TxHash
testSendTransaction = do
    outp <- head <$> replicateM 101 R.generate
    let Right (tx, _) = R.spendPackageOutputs [outp] (R.addrs !! 3) R.oneBitcoin
    sendTransaction tx Nothing

testMempoolAncestors :: BitcoindClient [TxHash]
testMempoolAncestors = testSendTransaction >>= getMempoolAncestors

testMempoolDescendants :: BitcoindClient [TxHash]
testMempoolDescendants = testSendTransaction >>= getMempoolAncestors

testCreateOutput :: BitcoindClient (OutPoint, Word64)
testCreateOutput = R.createOutput (R.addrs !! 4) (2 * R.oneBitcoin)

testRpc :: String -> BitcoindClient r -> (String, BitcoindClient ())
testRpc name x = (name, void x)

bitcoindTests :: [(String, BitcoindClient ())] -> NodeHandle -> IO ()
bitcoindTests ts h = do
    mgr <- newManager defaultManagerSettings
    let run msg = R.runBitcoind mgr h >=> assertRight msg
    mapM_ (uncurry run) ts

assertRight :: Show a => String -> Either a b -> IO b
assertRight msg = either onFail return
  where
    onFail e = assertFailure $ msg <> " - " <> show e
