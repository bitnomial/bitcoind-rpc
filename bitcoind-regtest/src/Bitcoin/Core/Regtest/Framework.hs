{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Bitcoin.Core.Regtest.Framework (
    -- * Run an ephemeral regtest node
    Version,
    NodeHandle (..),
    portsInUse,
    runBitcoind,
    withBitcoind,
    peerWith,

    -- * Command line
    dumpCommandLine,

    -- * Funding
    oneBitcoin,
    createOutput,
    generate,
    spendPackageOutputs,

    -- * Internal wallet

    --
    -- In the following lists, entries correspond to each other e.g. the p2pkh
    -- address for @keys !! 1@ is @addrs !! 1@.
    xprv,
    keys,
    pubKeys,
    addrs,
    textAddrs,

    -- * Versions
    v19_1,
    v20_0,
    v20_1,
    v21_0,
    v21_1,
    v22_0,
    v23_0,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (
    Exception,
    SomeException (SomeException),
    bracket,
    catch,
    throwIO,
 )
import Control.Monad (void)
import Data.Attoparsec.Text (char, decimal, parseOnly, sepBy, string)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin.Address (
    Address,
    addrToText,
    addressToOutput,
 )
import Haskoin.Block (blockTxns)
import Haskoin.Constants (btcTest)
import Haskoin.Crypto (SecKey)
import Haskoin.Keys (
    PubKeyI,
    XPrvKey (..),
    deriveAddrs,
    derivePubKeyI,
    deriveXPubKey,
    makeXPrvKey,
    prvSubKeys,
    wrapSecKey,
 )
import Haskoin.Script (sigHashAll)
import Haskoin.Transaction (
    OutPoint (..),
    SigInput (..),
    Tx (..),
    TxOut (..),
    buildAddrTx,
    signTx,
    txHash,
 )
import Haskoin.Util (encodeHex, maybeToEither)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.API (BasicAuthData (..))
import System.Directory (createDirectoryIfMissing)
import System.IO (Handle, IOMode (..), openFile)
import System.IO.Temp (getCanonicalTemporaryDirectory, withSystemTempDirectory)
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    readProcess,
    terminateProcess,
    waitForProcess,
 )

import Bitcoin.Core.RPC (
    BitcoindClient,
    BitcoindException,
    Command (Add),
    basicAuthFromCookie,
 )
import qualified Bitcoin.Core.RPC as RPC
import Data.Text.Encoding (decodeUtf8)

type Version = (Int, Int, Int)

-- | Data needed to connect to a @bitcoind-regtest@ ephemeral node
data NodeHandle = NodeHandle
    { nodeP2pPort :: Int
    , nodeRpcPort :: Int
    , nodeAuth :: BasicAuthData
    , nodeRawTx :: FilePath
    , nodeRawBlock :: FilePath
    , nodeVersion :: Version
    }

dumpCommandLine :: NodeHandle -> Text
dumpCommandLine h =
    Text.unwords
        [ "bitcoin-cli"
        , "-chain=regtest"
        , "-rpcport=" <> (Text.pack . show . nodeRpcPort) h
        , "-rpcuser=" <> (decodeUtf8 . basicAuthUsername . nodeAuth) h
        , "-rpcpassword=" <> (decodeUtf8 . basicAuthPassword . nodeAuth) h
        ]

portsInUse :: NodeHandle -> [Int]
portsInUse NodeHandle{nodeRpcPort} = [nodeRpcPort - 1, nodeRpcPort]

-- | Run an RPC computation with a node launched in this testing framework
runBitcoind :: Manager -> NodeHandle -> BitcoindClient r -> IO (Either BitcoindException r)
runBitcoind mgr NodeHandle{nodeRpcPort, nodeAuth} = RPC.runBitcoind mgr "127.0.0.1" nodeRpcPort nodeAuth

-- | Provide bracketed access to a fresh node
withBitcoind ::
    -- | Unused port
    Int ->
    -- | Optional data directory
    Maybe FilePath ->
    (NodeHandle -> IO r) ->
    IO r
withBitcoind basePort dataDir k = do
    v <- bitcoindVersion
    withDataDir $ \dd -> do
        createDirectoryIfMissing True $ dd <> "/regtest/wallets"
        tmp <- getCanonicalTemporaryDirectory
        bracket (initBitcoind tmp dd basePort) stopBitcoind . const $ do
            auth <-
                retryOnError 1_000_000
                    . basicAuthFromCookie
                    $ dd <> "/regtest/.cookie"
            let nodeHandle =
                    NodeHandle
                        basePort
                        (getRpcPort basePort)
                        auth
                        (rawTxSocket dd)
                        (rawBlockSocket dd)
                        v
            mgr <- newManager defaultManagerSettings
            waitForRPC mgr nodeHandle
            k nodeHandle
  where
    withDataDir onDataDir
        | Just dd <- dataDir = onDataDir dd
        | otherwise = withSystemTempDirectory "bitcoind-rpc-tests" onDataDir
    waitForRPC mgr nodeHandle =
        retryOnEither 1_000_000 $ runBitcoind mgr nodeHandle RPC.getBlockCount

retryOnError :: Int -> IO a -> IO a
retryOnError delay task =
    catch task $ \SomeException{} ->
        threadDelay delay >> retryOnError delay task

retryOnEither :: Int -> IO (Either e a) -> IO a
retryOnEither delay task =
    task >>= either onLeft pure
  where
    onLeft _ = threadDelay delay >> retryOnEither delay task

peerWith :: Manager -> NodeHandle -> NodeHandle -> IO ()
peerWith mgr nodeA nodeB =
    runBitcoind mgr nodeA (RPC.addNode nodeAddrB Add) >>= either throwIO pure
  where
    nodeAddrB = Text.pack $ "127.0.0.1:" <> show (nodeP2pPort nodeB)

initBitcoind :: FilePath -> FilePath -> Int -> IO ProcessHandle
initBitcoind tmp ddir basePort = do
    logH <- openFile (tmp <> "/bitcoind-rpc-" <> show basePort <> ".log") WriteMode
    (_, _, _, h) <- createProcess $ bitcoind ddir basePort logH
    pure h

stopBitcoind :: ProcessHandle -> IO ()
stopBitcoind h = terminateProcess h >> void (waitForProcess h)

bitcoindVersion :: IO Version
bitcoindVersion =
    readProcess "bitcoind" ["--version"] mempty
        >>= either (throwIO . ParseError) pure . parseVersion
  where
    parseVersion = parseOnly versionP . Text.pack

    versionP = do
        string "Bitcoin Core version v"
        decimal `sepBy` char '.' >>= \case
            vA : vB : vC : _ -> pure (vA, vB, vC)
            _ -> fail "Unable to parse version"

newtype BitcoindRegtestError = ParseError String
    deriving (Eq, Show)

instance Exception BitcoindRegtestError

bitcoind :: FilePath -> Int -> Handle -> CreateProcess
bitcoind ddir basePort output =
    (proc "bitcoind" args)
        { std_out = UseHandle output
        , std_err = UseHandle output
        }
  where
    args =
        [ "-chain=regtest"
        , "-txindex"
        , "-server"
        , "-blockfilterindex=1"
        , "-walletdir=" <> ddir <> "/regtest/wallets"
        , "-datadir=" <> ddir
        , "-port=" <> show (getPeerPort basePort)
        , "-rpcport=" <> show (getRpcPort basePort)
        , "-zmqpubrawblock=" <> rawBlockSocket ddir
        , "-zmqpubrawtx=" <> rawTxSocket ddir
        ]

getPeerPort :: Int -> Int
getPeerPort = id

getRpcPort :: Int -> Int
getRpcPort = (+ 1)

rawTxSocket :: FilePath -> String
rawTxSocket tmp = "ipc://" <> tmp <> "/bitcoind-rpc.tx.raw"

rawBlockSocket :: FilePath -> String
rawBlockSocket tmp = "ipc://" <> tmp <> "/bitcoind-rpc.block.raw"

oneBitcoin :: Word64
oneBitcoin = 100_000_000

-- | Funds an output with the minimum of the given amount and 100 blocks of subsidies
createOutput ::
    -- | address for the newly created output
    Address ->
    -- | target amount
    Word64 ->
    BitcoindClient (OutPoint, Word64)
createOutput addr vTarget = do
    inputs <- generateEnoughBlocks vTarget
    -- Make mined outputs spendable
    RPC.generateToAddress 100 textAddr2 Nothing

    let Right (tx, vFund) = spendPackageOutputs inputs addr vTarget
    h <- RPC.sendRawTransaction (encodeHex $ S.encode tx) Nothing
    RPC.generateToAddress 6 textAddr2 Nothing

    return (OutPoint h 0, vFund)

generateEnoughBlocks :: Word64 -> BitcoindClient [(OutPoint, Word64)]
generateEnoughBlocks vTarget = go ([], 0, 0 :: Int)
  where
    go (xs, v, n)
        | n >= 100 = return xs
        | otherwise = do
            x@(_, v0) <- generate
            let xs' = x : xs
            if v + v0 >= vTarget
                then return xs'
                else go (xs', v + v0, n + 1)

{- | A simplified block generator which does not require the tester to manage a
 wallet.  Use 'spendPackageOutputs' to spend.
-}
generate :: BitcoindClient (OutPoint, Word64)
generate =
    fmap (processCoinbase . head . blockTxns) $
        RPC.generateToAddress 1 textAddr0 Nothing >>= getBlock . head
  where
    getBlock h = RPC.getBlockBlock =<< RPC.getBlock h (Just 0)

processCoinbase :: Tx -> (OutPoint, Word64)
processCoinbase tx0 = (OutPoint (txHash tx0) 0, outValue . head $ txOut tx0)

-- | Spend outputs created by 'generate'
spendPackageOutputs ::
    -- | outputs produced by 'generate'
    [(OutPoint, Word64)] ->
    -- | recipient address
    Address ->
    -- | amount to spend
    Word64 ->
    Either String (Tx, Word64)
spendPackageOutputs inputs addr vTarget = do
    addrText <- maybeToEither "Addr conversion failed" $ addrToText btcTest addr

    let outSpec
            | vTarget + 10_000 < vAvail =
                Right ([(addrText, vTarget), (textAddr1, vAvail - vTarget - 10_000)], vTarget)
            | vAvail > 10_000 =
                Right ([(addrText, vAvail - 10_000)], vAvail - 10_000)
            | otherwise =
                Left "Insufficient funds"

        vAvail = sum $ snd <$> inputs
        sigIn (op, val) = SigInput (addressToOutput addr0) val op sigHashAll Nothing

    (outs, vFund) <- outSpec
    txSpec <- buildAddrTx btcTest (fst <$> inputs) outs
    tx <- signTx btcTest txSpec (sigIn <$> inputs) [key0]
    return (tx, vFund)

-- | Root key for the package wallet
xprv :: XPrvKey
xprv = makeXPrvKey "bitcoind-regtest key seed"

-- | Example secret keys
keys :: [SecKey]
keys = xPrvKey . fst <$> prvSubKeys xprv 0

-- | Example public keys
pubKeys :: [PubKeyI]
pubKeys = derivePubKeyI . wrapSecKey True <$> keys

key0 :: SecKey
key0 : _ = keys

-- | Example p2pkh addresses
addrs :: [Address]
addrs = repack <$> deriveAddrs (deriveXPubKey xprv) 0
  where
    repack (x, _, _) = x

addr0 :: Address
addr0 : _ = addrs

-- | Text versions of the example addresses
textAddrs :: [Text]
textAddrs = addrToText' <$> addrs
  where
    addrToText' a = let Just x = addrToText btcTest a in x

textAddr0, textAddr1, textAddr2 :: Text
textAddr0 : textAddr1 : textAddr2 : _ = textAddrs

v19_1, v20_0, v20_1, v21_0, v21_1, v22_0, v23_0 :: Version
v19_1 = (0, 19, 1)
v20_0 = (0, 20, 0)
v20_1 = (0, 20, 1)
v21_0 = (0, 21, 0)
v21_1 = (0, 21, 1)
v22_0 = (22, 0, 0)
v23_0 = (23, 0, 0)
