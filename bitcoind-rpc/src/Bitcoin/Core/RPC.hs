{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- |
 Module:      Bitcoin.Core.RPC
 Stability:   experimental

 We provide limited access to the bitcoin-core daemon RPC interface.  RPC
 method descriptions come from the bitcoind RPC help pages.

 /WARNING:/ The wallet RPC calls work for @bitcoind@ versions @0.21.0@ and above.
-}
module Bitcoin.Core.RPC (
    -- * Interacting with bitcoind
    BitcoindClient,
    runBitcoind,
    tryBitcoind,
    cookieClient,
    basicAuthFromCookie,
    mkBitcoindEnv,
    BitcoindException (..),

    -- * Transactions
    getRawTransaction,
    sendRawTransaction,
    sendTransaction,
    testMempoolAccept,

    -- * PSBT
    analyzePsbt,
    createPsbt,
    finalizePsbt,
    joinPsbts,
    utxoUpdatePsbt,

    -- * Blocks
    getBlock,
    getBlockFilter,
    getBlockHeader,
    getBlockHash,
    getBlockCount,
    getDifficulty,
    getBestBlockHash,
    getBlockStats,
    getChainTips,
    getChainTxStats,

    -- * Mempool
    getMempoolInfo,
    getMempoolAncestors,
    getMempoolDescendants,
    getRawMempool,

    -- * Network
    getPeerInfo,
    getConnectionCount,
    getNodeAddresses,
    getAddedNodeInfo,
    listBanned,
    getNetTotals,

    -- * Wallet
    abandonTransaction,
    abortRescan,
    AddressType (..),
    addMultisigAddress,
    backupWallet,
    bumpFee,
    createWallet,
    dumpPrivKey,
    dumpWallet,
    encryptWallet,
    Purpose (..),
    getAddressesByLabel,
    getAddressInfo,
    getBalance,
    getBalances,
    getNewAddress,
    getRawChangeAddress,
    getReceivedByAddress,
    getReceivedByLabel,
    getTransaction,
    getWalletInfo,
    importAddress,
    importDescriptors,
    ImportScriptPubKey (..),
    ImportMultiRequest (..),
    importMulti,
    importPrivKey,
    importPubKey,
    importWallet,
    listDescriptors,
    listLabels,
    listLockUnspent,
    listReceivedByAddress,
    listReceivedByLabel,
    listSinceBlock,
    listTransactions,
    listUnspent,
    listWallets,
    loadWallet,
    lockUnspent,
    psbtBumpFee,
    rescanBlockchain,
    FeeEstimationMode (..),
    sendMany,
    sendToAddress,
    setLabel,
    setTxFee,
    signMessage,
    signRawTx,
    unloadWallet,
    createFundedPsbt,
    walletLock,
    walletPassphrase,
    processPsbt,

    -- * Utilities
    estimateSmartFee,
    getDescriptorInfo,

    -- * Control
    stop,
    uptime,
    Command (..),
    addNode,
    disconnectNode,
    clearBanned,
    generateToAddress,

    -- * Response models
    module Bitcoin.Core.RPC.Responses,
) where

import Control.Monad (join)
import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)
import Data.Bifunctor (first, second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Client (Manager)
import Servant.API (BasicAuthData (..))
import Servant.Client (
    BaseUrl (..),
    ClientEnv,
    ClientError (FailureResponse),
    Scheme (..),
    mkClientEnv,
    responseBody,
    responseStatusCode,
    runClientM,
 )

import Bitcoin.Core.RPC.Blockchain
import Bitcoin.Core.RPC.Control
import Bitcoin.Core.RPC.Generating
import Bitcoin.Core.RPC.Network
import Bitcoin.Core.RPC.Responses
import Bitcoin.Core.RPC.Transactions
import Bitcoin.Core.RPC.Wallet
import Data.Aeson (Value)
import qualified Data.Aeson as Ae
import Network.HTTP.Types (statusCode)
import Servant.Bitcoind (
    BitcoindClient,
    BitcoindException (..),
 )
import Servant.Client.JsonRpc (JsonRpcErr (JsonRpcErr), JsonRpcResponse (Errors))

-- | Convenience function for sending a RPC call to bitcoind
runBitcoind ::
    Manager ->
    -- | host
    String ->
    -- | port
    Int ->
    BasicAuthData ->
    BitcoindClient a ->
    IO (Either BitcoindException a)
runBitcoind mgr host port auth =
    fmap consolidateErrors . (`runClientM` env) . runExceptT . (`runReaderT` auth)
  where
    env = mkBitcoindEnv mgr host port

{- | Convenience function for handling errors without leaving the
 'BitcoindClient' context
-}
tryBitcoind :: BitcoindClient a -> BitcoindClient (Either BitcoindException a)
tryBitcoind (ReaderT f) = ReaderT $ \x ->
    let ExceptT task = f x
     in ExceptT . fmap Right $ catchError task (pure . Left . decodeErrorResponse)

-- | Send a RPC call to bitcoind using credentials from a cookie file
cookieClient ::
    Manager ->
    -- | path to the cookie file
    FilePath ->
    -- | host
    String ->
    -- | port
    Int ->
    BitcoindClient r ->
    IO (Either BitcoindException r)
cookieClient mgr cookiePath host port go =
    liftIO (basicAuthFromCookie cookiePath)
        >>= flip (runBitcoind mgr host port) go

{- | Parse a username and password from a file.  The contents of the file
 should be exactly "username:password" (not base64 encoded).
-}
basicAuthFromCookie ::
    -- | path to the cookie file
    FilePath ->
    IO BasicAuthData
basicAuthFromCookie f = repack <$> BS.readFile f
  where
    repack = uncurry BasicAuthData . second (BS.drop 1) . BS8.break (== ':')

-- | Convenience function for connecting to bitcoind
mkBitcoindEnv ::
    Manager ->
    -- | bitcoind host
    String ->
    -- | bitcoind RPC port
    Int ->
    ClientEnv
mkBitcoindEnv mgr host port = mkClientEnv mgr $ BaseUrl Http host port ""

consolidateErrors :: Either ClientError (Either BitcoindException a) -> Either BitcoindException a
consolidateErrors = join . first decodeErrorResponse

decodeErrorResponse :: ClientError -> BitcoindException
decodeErrorResponse = \case
    FailureResponse _ response
        | (statusCode . responseStatusCode) response == 500
          , Just (Errors _ (JsonRpcErr _ message _)) <- (Ae.decode @(JsonRpcResponse Value Value) . responseBody) response ->
            RpcException message
    otherError -> ClientException otherError
