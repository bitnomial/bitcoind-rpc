{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Bitcoin.Core.Test.Misc (
    miscRPC,
) where

import Control.Monad (replicateM)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin.Address (Address (..), addrToText, pubKeyAddr)
import Haskoin.Block (Block (..), BlockHash)
import Haskoin.Constants (btcTest)
import Haskoin.Crypto (SecKey)
import Haskoin.Keys (
    PubKeyI,
    derivePubKeyI,
    secKey,
    wrapSecKey,
 )
import Haskoin.Transaction (OutPoint (..), Tx (..), TxHash, txHash)
import Network.HTTP.Client (Manager)
import Test.Tasty (TestTree, testGroup)

import Bitcoin.Core.RPC
import Bitcoin.Core.Regtest (NodeHandle)
import qualified Bitcoin.Core.Regtest as R
import Bitcoin.Core.Test.Utils (bitcoindTest, testRpc)

miscRPC :: Manager -> NodeHandle -> TestTree
miscRPC mgr h =
    testGroup "other-rpcs" $
        bitcoindTest mgr h
            <$> [ testRpc "generatetoaddress" testGenerate
                , testRpc "getbestblockhash" getBestBlockHash
                , testRpc "getblockhash" $ getBlockHash 1
                , testRpc "getblockfilter" testBlockFilter
                , testRpc "getblockheader" testBlockHeader
                , testRpc "getblock" testBlock
                , testRpc "getblockcount" getBlockCount
                , testRpc "getdifficulty" getDifficulty
                , testRpc "getchaintips" getChainTips
                , testRpc "getblockstats" testBlockStats
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
                , testRpc "getdescriptorinfo" $
                    getDescriptorInfo
                        "wpkh([d34db33f/84h/0h/0h]0279be667ef9dcbbac55a06295Ce870b07029Bfcdb2dce28d959f2815b16f81798)"
                , testRpc "listbanned" listBanned
                , testRpc "getnettotals" getNetTotals
                , testRpc "uptime" uptime
                , testRpc "addnode" $ addNode "127.0.0.1:8448" Add
                , testRpc "clearbanned" clearBanned
                , testRpc "estimatesmartfee" $ estimateSmartFee 6 Nothing
                ]

testGenerate :: BitcoindClient [BlockHash]
testGenerate = generateToAddress 120 addrText Nothing

key :: SecKey
Just key = secKey "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

pk :: PubKeyI
pk = derivePubKeyI $ wrapSecKey True key

addr :: Address
addr = pubKeyAddr pk

addrText :: Text
Just addrText = addrToText btcTest addr

testBlock :: BitcoindClient Block
testBlock = getBestBlockHash >>= getBlock'
  where
    getBlock' h = getBlockBlock <$> getBlock h (Just 0)

testBlockFilter :: BitcoindClient CompactFilter
testBlockFilter = getBestBlockHash >>= getBlockFilter

testBlockHeader :: BitcoindClient BlockHeader
testBlockHeader = getBestBlockHash >>= getBlockHeader

testBlockStats :: BitcoindClient BlockStats
testBlockStats = getBestBlockHash >>= \h -> getBlockStats h Nothing

testGetTransaction :: BitcoindClient Tx
testGetTransaction =
    getBestBlockHash >>= getBlock' >>= (`getRawTransaction` Nothing) . txHash . head . blockTxns
  where
    getBlock' h = getBlockBlock <$> getBlock h (Just 0)

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
