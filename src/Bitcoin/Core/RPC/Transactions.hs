{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Bitcoin.Core.RPC.Transactions
    ( getTransaction
    , sendRawTransaction
    , MempoolTestResult (..)
    , testMempoolAccept
    ) where

import           Data.Aeson                  (FromJSON (..), withObject, (.:),
                                              (.:?))
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import           Network.Haskoin.Block       (BlockHash)
import           Network.Haskoin.Transaction (Tx, TxHash)
import           Servant.API                 ((:<|>) (..))

import           Servant.Bitcoind            (BitcoindClient, BitcoindEndpoint,
                                              C, DefFalse, F, I, O,
                                              toBitcoindClient)


data MempoolTestResult = MempoolTestResult
    { testTxid     :: TxHash
    , txAccepted   :: Bool
    , rejectReason :: Maybe Text
    } deriving (Eq, Show)


instance FromJSON MempoolTestResult where
    parseJSON = withObject "MempoolTestResult" $ \o ->
        MempoolTestResult <$> o .: "txid" <*> o .: "allowed" <*> o .:? "reject-reason"


type RawTxRpc
    =    BitcoindEndpoint "sendrawtransaction" (I Text -> O Double -> C TxHash)
    :<|> BitcoindEndpoint "getrawtransaction" (I TxHash -> F DefFalse Bool -> O BlockHash -> C Tx)
    :<|> BitcoindEndpoint "testmempoolaccept" (I [Tx] -> O Double -> C [MempoolTestResult])


-- | Submit a raw transaction (serialized, hex-encoded) to local node and network.
sendRawTransaction :: Text -> Maybe Double -> BitcoindClient TxHash


-- | By default this function only works for mempool transactions. When called
-- with a blockhash argument, getrawtransaction will return the transaction if
-- the specified block is available and the transaction is found in that block.
-- When called without a blockhash argument, getrawtransaction will return the
-- transaction if it is in the mempool, or if -txindex is enabled and the
-- transaction is in a block in the blockchain.
getTransaction :: TxHash -> Maybe BlockHash -> BitcoindClient Tx


-- | Returns result of mempool acceptance tests indicating if the transactions
-- would be accepted by mempool.  This checks if the transaction violates the
-- consensus or policy rules.
testMempoolAccept :: [Tx] -> Maybe Double -> BitcoindClient [MempoolTestResult]


sendRawTransaction :<|> getTransaction :<|> testMempoolAccept = toBitcoindClient $ Proxy @RawTxRpc
