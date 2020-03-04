{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Concurrent          (threadDelay)
import           Control.Exception           (bracket)
import           Control.Monad               (void, (>=>))
import qualified Data.Serialize              as S
import           Data.Text                   (Text)
import           Network.Haskoin.Address     (Address (..), addrToString,
                                              pubKeyAddr)
import           Network.Haskoin.Block       (Block (..), BlockHash,
                                              BlockHeader)
import           Network.Haskoin.Constants   (btcTest)
import           Network.Haskoin.Crypto      (Hash160, SecKey)
import           Network.Haskoin.Keys        (PubKeyI, derivePubKeyI, secKey,
                                              wrapSecKey)
import           Network.Haskoin.Script      (ScriptOutput (..), sigHashAll)
import           Network.Haskoin.Transaction (OutPoint (..), SigInput (..),
                                              Tx (..), TxHash, TxOut (..),
                                              buildAddrTx, signTx, txHash)
import           Network.Haskoin.Util        (encodeHex)
import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)
import           Servant.API                 (BasicAuthData)
import           System.IO                   (Handle, IOMode (..), openFile)
import           System.IO.Temp              (withSystemTempDirectory)
import           System.Process              (CreateProcess (..), ProcessHandle,
                                              StdStream (..), createProcess,
                                              proc, terminateProcess,
                                              waitForProcess)
import           Test.Tasty                  (defaultMain)
import           Test.Tasty.HUnit            (assertFailure, testCase)

import           Bitcoin.Core.RPC


main :: IO ()
main
    = defaultMain . testCase "bitcoind-rpc" . withBitcoind
    $ bitcoindTests
        [ testRpc "generatetoaddress"     testGenerate
        , testRpc "getbestblockhash"      getBestBlockHash
        , testRpc "getblockhash"        $ getBlockHash 1
        , testRpc "getblockheader"        testBlockHeader
        , testRpc "getblock"              testBlock
        , testRpc "getblockcount"         getBlockCount
        , testRpc "getdifficulty"         getDifficulty
        , testRpc "getchaintips"          getChainTips
        , testRpc "getchaintxstats"     $ getChainTxStats Nothing Nothing
        , testRpc "getmempoolinfo"        getMempoolInfo
        , testRpc "getrawmempool"         getRawMempool
        , testRpc "getrawtransaction"     testGetTransaction
        , testRpc "sendrawtransaction"    testSendTransaction
        , testRpc "getmempoolancestors"   testMempoolAncestors
        , testRpc "getmempooldescendants" testMempoolDescendants
        , testRpc "getpeerinfo"           getPeerInfo
        , testRpc "getconnectioncount"    getConnectionCount
        , testRpc "getnodeaddresses"    $ getNodeAddresses (Just 10)
        , testRpc "getaddednodeinfo"    $ getAddedNodeInfo Nothing
        , testRpc "listbanned"            listBanned
        , testRpc "getnettotals"          getNetTotals
        , testRpc "uptime"                uptime
        , testRpc "addnode"             $ addNode "127.0.0.1:8448" Add
        , testRpc "clearbanned"           clearBanned
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
testGetTransaction
    = getBestBlockHash >>= getBlock >>= (`getTransaction` Nothing) . txHash . head . blockTxns


testSendTransaction :: BitcoindClient TxHash
testSendTransaction = do
    h   <- head <$> generateToAddress 120 addrText Nothing
    tx0 <- head . blockTxns <$> getBlock h
    let Right txSpec1 = buildAddrTx btcTest [outPoint] [(addrText, v - 10_000)]
        TxOut v _     = head $ txOut tx0
        outPoint      = OutPoint (txHash tx0) 0
        sigInput      = SigInput (PayPKHash addrHash) v outPoint sigHashAll Nothing
        Right tx1     = signTx btcTest txSpec1 [sigInput] [key]
    sendRawTransaction (encodeHex $ S.encode tx1) Nothing


testMempoolAncestors :: BitcoindClient [TxHash]
testMempoolAncestors = testSendTransaction >>= getMempoolAncestors


testMempoolDescendants :: BitcoindClient [TxHash]
testMempoolDescendants = testSendTransaction >>= getMempoolDescendants


testRpc :: String -> BitcoindClient r -> (String, BitcoindClient ())
testRpc name x = (name, void x)


bitcoindTests :: [(String, BitcoindClient ())] -> BasicAuthData -> String -> Int -> IO ()
bitcoindTests ts auth host port = do
    mgr <- newManager defaultManagerSettings
    let run msg = runBitcoind mgr host port auth >=> assertRight msg
    mapM_ (uncurry run) ts


assertRight :: Show a => String -> Either a b -> IO b
assertRight msg = either onFail return
    where
    onFail e = assertFailure $ msg <> " - " <> show e


withBitcoind :: (BasicAuthData -> String -> Int -> IO r) -> IO r
withBitcoind k = withSystemTempDirectory "bitcoind-rpc-tests" $ \dd ->
    bracket (initBitcoind dd port) stopBitcoind . const $ do
        auth <- basicAuthFromCookie $ dd <> "/regtest/.cookie"
        k auth "127.0.0.1" port
    where
    port           = 8449
    stopBitcoind h = terminateProcess h >> void (waitForProcess h)


initBitcoind :: FilePath -> Int -> IO ProcessHandle
initBitcoind ddir port = do
    logH         <- openFile "/tmp/bitcoind-rpc.log" WriteMode
    (_, _, _, h) <- createProcess $ bitcoind ddir port logH
    threadDelay 1_000_000
    return h


bitcoind :: FilePath -> Int -> Handle -> CreateProcess
bitcoind ddir port output
    = (proc "bitcoind" args)
      { std_out = UseHandle output
      , std_err = UseHandle output
      }
    where
    args = ["-regtest", "-txindex", "-disablewallet", "-datadir=" <> ddir, "-rpcport=" <> show port]
