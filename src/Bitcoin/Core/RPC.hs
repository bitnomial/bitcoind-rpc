-- |
-- Module:      Bitcoin.Core.RPC
-- Stability:   experimental
--
-- We provide limited access to the bitcoin-core daemon RPC interface.  RPC
-- method descriptions come from the bitcoind RPC help pages.
module Bitcoin.Core.RPC
    (
    -- * Interacting with bitcoind
      BitcoindClient
    , runBitcoind
    , cookieClient
    , basicAuthFromCookie
    , mkBitcoindEnv
    , BitcoindException (..)

    -- * Transactions
    , getTransaction
    , sendRawTransaction
    , testMempoolAccept

    -- * Blocks
    , getBlock
    , getBlockHeader
    , getBlockHash
    , getBlockCount
    , getDifficulty

    , getBestBlockHash
    , getBlockStats
    , getChainTips
    , getChainTxStats

    -- * Mempool
    , getMempoolInfo
    , getMempoolAncestors
    , getMempoolDescendants
    , getRawMempool

    -- * Network
    , getPeerInfo
    , getConnectionCount
    , getNodeAddresses
    , getAddedNodeInfo
    , listBanned
    , getNetTotals

    -- * Control
    , stop
    , uptime
    , Command (..)
    , addNode
    , disconnectNode
    , clearBanned
    , generateToAddress

    -- * Response models
    , module Bitcoin.Core.RPC.Responses
    ) where

import           Control.Monad                 (join)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Except    (runExceptT)
import           Control.Monad.Trans.Reader    (runReaderT)
import           Data.Bifunctor                (first, second)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import           Network.HTTP.Client           (Manager)
import           Servant.API                   (BasicAuthData (..))
import           Servant.Client                (BaseUrl (..), ClientEnv,
                                                ClientError, Scheme (..),
                                                mkClientEnv, runClientM)

import           Bitcoin.Core.RPC.Blockchain
import           Bitcoin.Core.RPC.Control
import           Bitcoin.Core.RPC.Generating
import           Bitcoin.Core.RPC.Network
import           Bitcoin.Core.RPC.Responses
import           Bitcoin.Core.RPC.Transactions
import           Servant.Bitcoind              (BitcoindClient,
                                                BitcoindException (..))


-- | Convenience function for sending a RPC call to bitcoind
runBitcoind
    :: Manager
    -> String
    -- ^ host
    -> Int
    -- ^ port
    -> BasicAuthData
    -> BitcoindClient a
    -> IO (Either BitcoindException a)
runBitcoind mgr host port auth
    = fmap consolidateErrors . (`runClientM` env) . runExceptT . (`runReaderT` auth)
    where
    env = mkBitcoindEnv mgr host port


-- | Send a RPC call to bitcoind using credentials from a cookie file
cookieClient
    :: Manager
    -> FilePath
    -- ^ path to the cookie file
    -> String
    -- ^ host
    -> Int
    -- ^ port
    -> BitcoindClient r
    -> IO (Either BitcoindException r)
cookieClient mgr cookiePath host port go
    =   liftIO (basicAuthFromCookie cookiePath)
    >>= flip (runBitcoind mgr host port) go


-- | Parse a username and password from a file.  The contents of the file
-- should be exactly "username:password" (not base64 encoded).
basicAuthFromCookie
    :: FilePath
    -- ^ path to the cookie file
    -> IO BasicAuthData
basicAuthFromCookie f = repack <$> BS.readFile f
    where
    repack = uncurry BasicAuthData . second (BS.drop 1) . BS8.break (== ':')


-- | Convenience function for connecting to bitcoind
mkBitcoindEnv
    :: Manager
    -> String
    -- ^ bitcoind host
    -> Int
    -- ^ bitcoind RPC port
    -> ClientEnv
mkBitcoindEnv mgr host port = mkClientEnv mgr $ BaseUrl Http host port ""


consolidateErrors :: Either ClientError (Either BitcoindException a) -> Either BitcoindException a
consolidateErrors = join . first ClientException
