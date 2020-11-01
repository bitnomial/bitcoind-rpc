{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Core.Regtest (
    -- * Run an ephemeral regtest node
    NodeHandle (..),
    runBitcoind,
    withBitcoind,

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
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.Serialize as S
import Data.Text (Text)
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
import Network.HTTP.Client (Manager)
import Servant.API (BasicAuthData)
import System.IO (Handle, IOMode (..), openFile)
import System.IO.Temp (getCanonicalTemporaryDirectory, withSystemTempDirectory)
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
 )

import Bitcoin.Core.RPC (
    BitcoindClient,
    BitcoindException,
    basicAuthFromCookie,
 )
import qualified Bitcoin.Core.RPC as RPC

-- | Data needed to connect to a @bitcoind-regtest@ ephemeral node
data NodeHandle = NodeHandle
    { nodePort :: Int
    , nodeAuth :: BasicAuthData
    , nodeRawTx :: FilePath
    , nodeRawBlock :: FilePath
    }

-- | Run an RPC computation with an ephemeral node
runBitcoind :: Manager -> NodeHandle -> BitcoindClient r -> IO (Either BitcoindException r)
runBitcoind mgr (NodeHandle port auth _ _) = RPC.runBitcoind mgr "127.0.0.1" port auth

-- | Provide bracketed access to a fresh ephemeral node
withBitcoind ::
    -- | node port
    Int ->
    (NodeHandle -> IO r) ->
    IO r
withBitcoind port k = withSystemTempDirectory "bitcoind-rpc-tests" $ \dd -> do
    tmp <- getCanonicalTemporaryDirectory
    bracket (initBitcoind tmp dd port) stopBitcoind . const $ do
        auth <- basicAuthFromCookie $ dd <> "/regtest/.cookie"
        k $ NodeHandle port auth (rawTxSocket tmp) (rawBlockSocket tmp)

initBitcoind :: FilePath -> FilePath -> Int -> IO ProcessHandle
initBitcoind tmp ddir port = do
    logH <- openFile (tmp <> "/bitcoind-rpc.log") WriteMode
    (_, _, _, h) <- createProcess $ bitcoind tmp ddir port logH
    h <$ threadDelay 1_000_000

stopBitcoind :: ProcessHandle -> IO ()
stopBitcoind h = terminateProcess h >> void (waitForProcess h)

bitcoind :: FilePath -> FilePath -> Int -> Handle -> CreateProcess
bitcoind tmp ddir port output =
    (proc "bitcoind" args)
        { std_out = UseHandle output
        , std_err = UseHandle output
        }
  where
    args =
        [ "-regtest"
        , "-txindex"
        , "-blockfilterindex=1"
        , "-disablewallet"
        , "-datadir=" <> ddir
        , "-rpcport=" <> show port
        , "-zmqpubrawblock=" <> rawBlockSocket tmp
        , "-zmqpubrawtx=" <> rawTxSocket tmp
        ]

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
        RPC.generateToAddress 1 textAddr0 Nothing >>= RPC.getBlock . head
  where
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
