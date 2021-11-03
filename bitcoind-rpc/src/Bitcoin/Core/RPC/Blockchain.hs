{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bitcoin.Core.RPC.Blockchain (
    getBestBlockHash,
    getBlock,
    getBlockCount,
    getBlockHash,
    CompactFilter (..),
    getBlockFilter,
    BlockHeader (..),
    getBlockHeader,
    BlockStats (..),
    getBlockStats,
    ChainTip (..),
    ChainTipStatus (..),
    getChainTips,
    ChainTxStats (..),
    getChainTxStats,
    getDifficulty,
    getMempoolAncestors,
    getMempoolDescendants,
    MempoolInfo (..),
    getMempoolInfo,
    getRawMempool,
) where

import Bitcoin.CompactFilter (BlockFilter, BlockFilterHeader)
import Data.Aeson (
    FromJSON (..),
    withObject,
    withText,
    (.:),
    (.:?),
 )
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word16, Word32, Word64)
import Haskoin.Block (Block, BlockHash, BlockHeight)
import Haskoin.Crypto (Hash256)
import Haskoin.Transaction (TxHash)
import Servant.API ((:<|>) (..))

import Data.Aeson.Utils (
    HexEncoded (unHexEncoded),
    decodeFromHex,
    toSatoshis,
    utcTime,
 )
import Servant.Bitcoind (
    BitcoindClient,
    BitcoindEndpoint,
    C,
    DefFalse,
    DefTrue,
    DefZero,
    F,
    I,
    O,
    toBitcoindClient,
 )

data BlockStats = BlockStats
    { blockStatsAvgFee :: Double
    , blockStatsAvgFeeRate :: Word32
    , blockStatsAvgTxSize :: Word32
    , blockStatsBlockHash :: BlockHash
    , blockStatsFeeRatePercentiles :: [Word32]
    , blockStatsHeight :: BlockHeight
    , blockStatsIns :: Word32
    , blockStatsMaxFee :: Word32
    , blockStatsMaxFeeRate :: Word32
    , blockStatsMinTxSize :: Word32
    , blockStatsOuts :: Word32
    , blockStatsSubsidy :: Word64
    , blockStatsSegwitSize :: Word32
    , blockStastSegwitWeight :: Word32
    , blockStatsSegwitCount :: Word32
    , blockStatsTime :: UTCTime
    , blockStatsTotalOut :: Word32
    , blockStatsTotalSize :: Word32
    , blockStatsTotalWeight :: Word32
    , blockStatsTotalFee :: Word32
    , blockStatsCount :: Word32
    , blockStatsUtxoIncrease :: Int
    , blockStatsUtxoSizeIncrease :: Int
    }
    deriving (Eq, Show)

instance FromJSON BlockStats where
    parseJSON = withObject "BlockStats" $ \o ->
        BlockStats
            <$> o .: "avgfee"
            <*> o .: "avgfeerate"
            <*> o .: "avgtxsize"
            <*> o .: "blockhash"
            <*> o .: "feerate_percentiles"
            <*> o .: "height"
            <*> o .: "ins"
            <*> o .: "maxfee"
            <*> o .: "maxfeerate"
            <*> o .: "mintxsize"
            <*> o .: "outs"
            <*> o .: "subsidy"
            <*> o .: "swtotal_size"
            <*> o .: "swtotal_weight"
            <*> o .: "swtxs"
            <*> (utcTime <$> o .: "time")
            <*> o .: "total_out"
            <*> o .: "total_size"
            <*> o .: "total_weight"
            <*> o .: "totalfee"
            <*> o .: "txs"
            <*> o .: "utxo_increase"
            <*> o .: "utxo_size_inc"

data CompactFilter = CompactFilter
    { filterHeader :: BlockFilterHeader
    , filterBody :: BlockFilter
    }

instance FromJSON CompactFilter where
    parseJSON = withObject "CompactFilter" $ \o ->
        CompactFilter
            <$> (o .: "header" >>= parseFromHex)
            <*> (o .: "filter" >>= parseFromHex)

data BlockHeader = BlockHeader
    { blockHeaderHash :: BlockHash
    , blockHeaderConfs :: Word32
    , blockHeaderHeight :: BlockHeight
    , blockHeaderMerkleRoot :: Hash256
    , blockHeaderTime :: UTCTime
    , blockHeaderMedianTime :: UTCTime
    , blockHeaderNonce :: Word64
    , blockHeaderTxCount :: Int
    , blockHeaderPrevHash :: Maybe Hash256
    , blockHeaderNextHash :: Maybe Hash256
    }

instance FromJSON BlockHeader where
    parseJSON = withObject "BlockHeader" $ \o ->
        BlockHeader
            <$> o .: "hash"
            <*> o .: "confirmations"
            <*> o .: "height"
            <*> (o .: "merkleroot" >>= parseFromHex)
            <*> (utcTime <$> o .: "time")
            <*> (utcTime <$> o .: "mediantime")
            <*> o .: "nonce"
            <*> o .: "nTx"
            <*> (o .:? "previousblockhash" >>= traverse parseFromHex)
            <*> (o .:? "nextblockhash" >>= traverse parseFromHex)

parseFromHex :: Serialize a => Text -> Parser a
parseFromHex = either fail return . decodeFromHex

data ChainTipStatus = Invalid | HeadersOnly | ValidHeaders | ValidFork | Active
    deriving (Eq, Show)

instance FromJSON ChainTipStatus where
    parseJSON = withText "ChainTipStatus" chainTipStatus
      where
        chainTipStatus t
            | t == "invalid" = return Invalid
            | t == "headers-only" = return HeadersOnly
            | t == "valid-headers" = return ValidHeaders
            | t == "valid-fork" = return ValidFork
            | t == "active" = return Active
            | otherwise = fail "Unknown chain tip status"

data ChainTip = ChainTip
    { tipHeight :: Word32
    , tipHash :: BlockHash
    , branchLength :: Word16
    , tipStatus :: ChainTipStatus
    }
    deriving (Eq, Show)

instance FromJSON ChainTip where
    parseJSON = withObject "ChainTip" $ \o ->
        ChainTip <$> o .: "height" <*> o .: "hash" <*> o .: "branchlen" <*> o .: "status"

data ChainTxStats = ChainTxStats
    { txStatsTime :: UTCTime
    , txCount :: Word32
    , finalBlockHash :: BlockHash
    , finalBlockHeight :: BlockHeight
    , finalBlockCount :: Word32
    , windowTxCount :: Maybe Word32
    , windowInterval :: Maybe NominalDiffTime
    , txRate :: Maybe Double
    }
    deriving (Eq, Show)

instance FromJSON ChainTxStats where
    parseJSON = withObject "ChainTxStats" $ \o ->
        ChainTxStats
            <$> (utcTime <$> o .: "time")
            <*> o .: "txcount"
            <*> o .: "window_final_block_hash"
            <*> o .: "window_final_block_height"
            <*> o .: "window_block_count"
            <*> o .:? "window_tx_count"
            <*> o .:? "window_interval"
            <*> o .:? "txrate"

data MempoolInfo = MempoolInfo
    { mempoolLoaded :: Bool
    , mempoolSize :: Word32
    , mempoolBytes :: Word32
    , mempoolUsage :: Word32
    , mempoolMax :: Word32
    , mempoolMinFee :: Word64
    , mempoolMinRelayFee :: Word64
    }
    deriving (Eq, Show)

instance FromJSON MempoolInfo where
    parseJSON = withObject "MempoolInfo" $ \o ->
        MempoolInfo
            <$> o .: "loaded"
            <*> o .: "size"
            <*> o .: "bytes"
            <*> o .: "usage"
            <*> o .: "maxmempool"
            <*> (toSatoshis <$> o .: "mempoolminfee")
            <*> (toSatoshis <$> o .: "minrelaytxfee")

type BlockchainRpc =
    BitcoindEndpoint "getbestblockhash" (C BlockHash)
        :<|> BitcoindEndpoint "getblock" (I BlockHash -> F DefZero Int -> C (HexEncoded Block))
        :<|> BitcoindEndpoint "getblockcount" (C Word32)
        :<|> BitcoindEndpoint "getblockfilter" (I BlockHash -> C CompactFilter)
        :<|> BitcoindEndpoint "getblockhash" (I BlockHeight -> C BlockHash)
        :<|> BitcoindEndpoint "getblockheader" (I BlockHash -> F DefTrue Bool -> C BlockHeader)
        :<|> BitcoindEndpoint "getblockstats" (I BlockHash -> O [Text] -> C BlockStats)
        :<|> BitcoindEndpoint "getchaintips" (C [ChainTip])
        :<|> BitcoindEndpoint "getchaintxstats" (O Word32 -> O BlockHash -> C ChainTxStats)
        :<|> BitcoindEndpoint "getdifficulty" (C Scientific)
        :<|> BitcoindEndpoint "getmempoolancestors" (I TxHash -> F DefFalse Bool -> C [TxHash])
        :<|> BitcoindEndpoint "getmempooldescendants" (I TxHash -> F DefFalse Bool -> C [TxHash])
        :<|> BitcoindEndpoint "getmempoolinfo" (C MempoolInfo)
        :<|> BitcoindEndpoint "getrawmempool" (F DefFalse Bool -> C [TxHash])

-- | Returns the hash of the best (tip) block in the most-work fully-validated chain.
getBestBlockHash :: BitcoindClient BlockHash
getBlock' :: BlockHash -> BitcoindClient (HexEncoded Block)

-- | Returns the height of the most-work fully-validated chain.  The genesis block has height 0.
getBlockCount :: BitcoindClient Word32

-- | Returns hash of block in best-block-chain at height provided.
getBlockHash :: BlockHeight -> BitcoindClient BlockHash

-- | Retrieve a BIP 157 content filter for a particular block.
getBlockFilter :: BlockHash -> BitcoindClient CompactFilter

-- | Returns the header of the block corresponding to the given 'BlockHash'
getBlockHeader :: BlockHash -> BitcoindClient BlockHeader
getBlockStats' :: BlockHash -> Maybe [Text] -> BitcoindClient BlockStats

{- | Return information about all known tips in the block tree, including the
 main chain as well as orphaned branches.
-}
getChainTips :: BitcoindClient [ChainTip]

-- | Compute statistics about the total number and rate of transactions in the chain.
getChainTxStats :: Maybe Word32 -> Maybe BlockHash -> BitcoindClient ChainTxStats

-- | Returns the proof-of-work difficulty as a multiple of the minimum difficulty.
getDifficulty :: BitcoindClient Scientific

-- | If txid is in the mempool, returns all in-mempool ancestors.
getMempoolAncestors :: TxHash -> BitcoindClient [TxHash]

-- | If txid is in the mempool, returns all in-mempool descendants.
getMempoolDescendants :: TxHash -> BitcoindClient [TxHash]

-- | Returns details on the active state of the TX memory pool.
getMempoolInfo :: BitcoindClient MempoolInfo

-- | Returns all transaction ids in memory pool.
getRawMempool :: BitcoindClient [TxHash]
getBestBlockHash
    :<|> getBlock'
    :<|> getBlockCount
    :<|> getBlockFilter
    :<|> getBlockHash
    :<|> getBlockHeader
    :<|> getBlockStats'
    :<|> getChainTips
    :<|> getChainTxStats
    :<|> getDifficulty
    :<|> getMempoolAncestors
    :<|> getMempoolDescendants
    :<|> getMempoolInfo
    :<|> getRawMempool =
        toBitcoindClient $ Proxy @BlockchainRpc

{- | Compute per block statistics for a given window. All amounts are in
 satoshis.  It won't work for some heights with pruning.
-}
getBlockStats :: BlockHash -> BitcoindClient BlockStats
getBlockStats h = getBlockStats' h Nothing

-- | Produce the block corresponding to the given 'BlockHash' if it exists.
getBlock :: BlockHash -> BitcoindClient Block
getBlock = fmap unHexEncoded . getBlock'
