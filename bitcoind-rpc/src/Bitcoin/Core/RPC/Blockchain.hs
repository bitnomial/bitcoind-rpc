{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bitcoin.Core.RPC.Blockchain (
    getBestBlockHash,
    getBlock,
    getBlockBlock,
    getBlockCount,
    getBlockHash,
    getBlockResponse,
    GetBlockResponse (..),
    GetBlockV2Response (..),
    CompactFilter (..),
    getBlockFilter,
    BlockHeader (..),
    getBlockHeader,
    BlockStat (..),
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
    ToJSON (..),
    Value (String),
    withObject,
    withText,
    (.:),
    (.:?),
 )
import Data.Aeson.Types (Parser)
import Data.Aeson.Utils (
    HexEncoded (unHexEncoded),
    decodeFromHex,
    toSatoshis,
    utcTime,
 )
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word16, Word32, Word64)
import qualified Haskoin as H
import Haskoin.Block (Block (..), BlockHash, BlockHeight, hexToBlockHash)
import Haskoin.Crypto (Hash256)
import Haskoin.Transaction (Tx, TxHash)
import Servant.API ((:<|>) (..))
import Servant.Bitcoind (
    BitcoindClient,
    BitcoindEndpoint,
    C,
    DefFalse,
    DefTrue,
    F,
    I,
    O,
    toBitcoindClient,
 )

{- | Values to plot.

These are labels for the statistics accepted in the 'getblockstat' RPC command.
-}
data BlockStat
    = -- | Selects the average fee in the block statistic
      StatAvgFee
    | -- | Selects the average feerate (in satoshis per virtual byte) statistic
      StatAvgFeeRate
    | -- | Selects the average transaction size statistic
      StatAvgTxSize
    | -- | Selects the block hash (to check for potential reorgs)
      StatBlockHash
    | -- | Selects feerates statistics at the 10th, 25th, 50th, 75th, and 90th percentile weight unit (in satoshis per virtual byte)
      StatFeeRatePercentiles
    | -- | Selects the height of the block
      StatHeight
    | -- | Selects the number of inputs (excluding coinbase) statistic
      StatInputs
    | -- | Selects the maximum fee in the block statistic
      StatMaxFee
    | -- | Selects the maximum feerate (in satoshis per virtual byte) statistic
      StatMaxFeeRate
    | -- | Selects the maximum transaction size statistic
      StatMaxTxSize
    | -- | Selects the truncated median fee in the block statistic
      StatMedianFee
    | -- | Selects the block median time past statistic
      StatMedianTime
    | -- | Selects the truncated median transaction size statistic
      StatMedianTxSize
    | -- | Selects the minimum fee in the block statistic
      StatMinFee
    | -- | Selects the minimum feerate (in satoshis per virtual byte) statistic
      StatMinFeeRate
    | -- | Selects the minimum transaction size statistic
      StatMinTxSize
    | -- | Selects the number of outputs statistic
      StatOutputs
    | -- | Selects the block subsidy
      StatSubsidy
    | -- | Selects the total size of all segwit transactions statistic
      StatSegwitTotalSize
    | -- | Selects the total weight of all segwit transactions statistic
      StatSegwitTotalWeight
    | -- | Selects the number of segwit transactions statistic
      StatSegwitTxs
    | -- | Selects the block time
      StatTime
    | -- | Selects the total amount in all outputs (excluding coinbase and thus reward [ie subsidy + totalfee]) statistic
      StatTotalOut
    | -- | Selects the total size of all non-coinbase transactions statistic
      StatTotalSize
    | -- | Selects the total weight of all non-coinbase transactions statistic
      StatTotalWeight
    | -- | Selects the fee total statistic
      StatTotalFee
    | -- | Selects the number of transactions (including coinbase) statistic
      StatTxs
    | -- | Selects the increase/decrease in the number of unspent outputs statistic
      StatUtxoIncrease
    | -- | Selects the increase/decrease in size for the utxo index (not discounting op_return and similar) statistic
      StatUtxoSizeIncrease
    deriving (Eq, Show)

instance ToJSON BlockStat where
    toJSON = \case
        StatAvgFee -> "avgfee"
        StatAvgFeeRate -> "avgfeerate"
        StatAvgTxSize -> "avgtxsize"
        StatBlockHash -> "blockhash"
        StatFeeRatePercentiles -> "feerate_percentiles"
        StatHeight -> "height"
        StatInputs -> "ins"
        StatMaxFee -> "maxfee"
        StatMaxFeeRate -> "maxfeerate"
        StatMaxTxSize -> "maxtxsize"
        StatMedianFee -> "medianfee"
        StatMedianTime -> "mediantime"
        StatMedianTxSize -> "mediantxsize"
        StatMinFee -> "minfee"
        StatMinFeeRate -> "minfeerate"
        StatMinTxSize -> "mintxsize"
        StatOutputs -> "outs"
        StatSubsidy -> "subsidy"
        StatSegwitTotalSize -> "swtotal_size"
        StatSegwitTotalWeight -> "swtotal_weight"
        StatSegwitTxs -> "swtxs"
        StatTime -> "time"
        StatTotalOut -> "total_out"
        StatTotalSize -> "total_size"
        StatTotalWeight -> "total_weight"
        StatTotalFee -> "totalfee"
        StatTxs -> "txs"
        StatUtxoIncrease -> "utxo_increase"
        StatUtxoSizeIncrease -> "utxo_size_inc"

data BlockStats = BlockStats
    { blockStatsAvgFee :: Maybe Double
    , blockStatsAvgFeeRate :: Maybe Word32
    , blockStatsAvgTxSize :: Maybe Word32
    , blockStatsBlockHash :: Maybe BlockHash
    , blockStatsFeeRatePercentiles :: Maybe [Word32]
    , blockStatsHeight :: Maybe BlockHeight
    , blockStatsIns :: Maybe Word32
    , blockStatsMaxFee :: Maybe Word64
    , blockStatsMaxFeeRate :: Maybe Word32
    , blockStatsMaxTxSize :: Maybe Word32
    , blockStatsMedianFee :: Maybe Word64
    , blockStatsMedianTime :: Maybe UTCTime
    , blockStatsMedianTxSize :: Maybe Word32
    , blockStatsMinFee :: Maybe Word64
    , blockStatsMinFeeRate :: Maybe Word32
    , blockStatsMinTxSize :: Maybe Word32
    , blockStatsOuts :: Maybe Word32
    , blockStatsSubsidy :: Maybe Word64
    , blockStatsSegwitSize :: Maybe Word32
    , blockStastSegwitWeight :: Maybe Word32
    , blockStatsSegwitCount :: Maybe Word32
    , blockStatsTime :: Maybe UTCTime
    , blockStatsTotalOut :: Maybe Int64
    , blockStatsTotalSize :: Maybe Word32
    , blockStatsTotalWeight :: Maybe Word32
    , blockStatsTotalFee :: Maybe Word64
    , blockStatsCount :: Maybe Word32
    , blockStatsUtxoIncrease :: Maybe Int
    , blockStatsUtxoSizeIncrease :: Maybe Int
    }
    deriving (Eq, Show)

instance FromJSON BlockStats where
    parseJSON = withObject "BlockStats" $ \o ->
        BlockStats
            <$> o .:? "avgfee"
            <*> o .:? "avgfeerate"
            <*> o .:? "avgtxsize"
            <*> o .:? "blockhash"
            <*> o .:? "feerate_percentiles"
            <*> o .:? "height"
            <*> o .:? "ins"
            <*> o .:? "maxfee"
            <*> o .:? "maxfeerate"
            <*> o .:? "maxtxsize"
            <*> o .:? "medianfee"
            <*> (fmap utcTime <$> o .:? "mediantime")
            <*> o .:? "mediantxsize"
            <*> o .:? "minfee"
            <*> o .:? "minfeerate"
            <*> o .:? "mintxsize"
            <*> o .:? "outs"
            <*> o .:? "subsidy"
            <*> o .:? "swtotal_size"
            <*> o .:? "swtotal_weight"
            <*> o .:? "swtxs"
            <*> (fmap utcTime <$> o .:? "time")
            <*> o .:? "total_out"
            <*> o .:? "total_size"
            <*> o .:? "total_weight"
            <*> o .:? "totalfee"
            <*> o .:? "txs"
            <*> o .:? "utxo_increase"
            <*> o .:? "utxo_size_inc"

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
    , blockHeaderDifficulty :: Double
    , blockHeaderTxCount :: Int
    , blockHeaderPrevHash :: Maybe BlockHash
    , blockHeaderNextHash :: Maybe BlockHash
    }
    deriving (Eq, Show)

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
            <*> o .: "difficulty"
            <*> o .: "nTx"
            <*> (o .:? "previousblockhash" >>= traverse parseBlockHash)
            <*> (o .:? "nextblockhash" >>= traverse parseBlockHash)
      where
        parseBlockHash = maybe (fail "Invalid hex in block hash") pure . hexToBlockHash

parseFromHex :: (Serialize a) => Text -> Parser a
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
        :<|> BitcoindEndpoint "getblock" (I BlockHash -> O Int -> C GetBlockResponse)
        :<|> BitcoindEndpoint "getblockcount" (C Word32)
        :<|> BitcoindEndpoint "getblockfilter" (I BlockHash -> C CompactFilter)
        :<|> BitcoindEndpoint "getblockhash" (I BlockHeight -> C BlockHash)
        :<|> BitcoindEndpoint "getblockheader" (I BlockHash -> F DefTrue Bool -> C BlockHeader)
        :<|> BitcoindEndpoint "getblockstats" (I BlockHash -> O [BlockStat] -> C BlockStats)
        :<|> BitcoindEndpoint "getchaintips" (C [ChainTip])
        :<|> BitcoindEndpoint "getchaintxstats" (O Word32 -> O BlockHash -> C ChainTxStats)
        :<|> BitcoindEndpoint "getdifficulty" (C Scientific)
        :<|> BitcoindEndpoint "getmempoolancestors" (I TxHash -> F DefFalse Bool -> C [TxHash])
        :<|> BitcoindEndpoint "getmempooldescendants" (I TxHash -> F DefFalse Bool -> C [TxHash])
        :<|> BitcoindEndpoint "getmempoolinfo" (C MempoolInfo)
        :<|> BitcoindEndpoint "getrawmempool" (F DefFalse Bool -> C [TxHash])

-- | Returns the hash of the best (tip) block in the most-work fully-validated chain.
getBestBlockHash :: BitcoindClient BlockHash

-- TODO add support for other verbosity values
data GetBlockResponse
    = GetBlockV0 Block
    | GetBlockV2 GetBlockV2Response
    deriving (Eq, Show)

instance FromJSON GetBlockResponse where
    parseJSON v@(String _) = GetBlockV0 . unHexEncoded <$> parseJSON v
    parseJSON v = GetBlockV2 <$> parseJSON v

data GetBlockV2Response = GetBlockV2Response
    { getBlockV2Hash :: BlockHash
    , getBlockV2Confirmations :: Int
    , getBlockV2Height :: BlockHeight
    , getBlockV2Version :: Word32
    , getBlockV2PreviousBlockHash :: Maybe BlockHash
    , getBlockV2NextBlockHash :: Maybe BlockHash
    , getBlockV2MerkleRoot :: Hash256
    , getBlockV2Time :: UTCTime
    , getBlockV2Bits :: Word32
    , getBlockV2Nonce :: Word32
    , getBlockV2Txs :: [(TxHash, Tx, Maybe Word64)]
    -- ^ Includes coinbase transaction which does not have a fee
    }
    deriving (Eq, Show)

instance FromJSON GetBlockV2Response where
    parseJSON = withObject "GetBlockV2Response" $ \o -> do
        GetBlockV2Response
            <$> o .: "hash"
            <*> o .: "confirmations"
            <*> o .: "height"
            <*> o .: "version"
            <*> o .:? "previousblockhash"
            <*> o .:? "nextblockhash"
            <*> (o .: "merkleroot" >>= parseFromHex)
            <*> (utcTime <$> o .: "time")
            <*> (unHexEncoded <$> o .: "bits")
            <*> o .: "nonce"
            <*> (mapM parseTxAndFee =<< (o .: "tx"))
      where
        parseTxAndFee = withObject "GetBlockV2Response.tx" $ \o ->
            (,,)
                <$> o .: "txid"
                <*> (unHexEncoded <$> o .: "hex")
                <*> (fmap toSatoshis <$> o .:? "fee")

-- | Eliminator for the 'GetblockResponse'
getBlockResponse :: (Block -> a) -> (GetBlockV2Response -> a) -> GetBlockResponse -> a
getBlockResponse f g = \case
    GetBlockV0 block -> f block
    GetBlockV2 response -> g response

{- | Returns a block. If verbosity is 0, returns a 'Block' decoded from the
underlying hex-encoded data. If verbosity is 2, returns an object with
information about block and information about each transaction.
-}
getBlock :: BlockHash -> Maybe Int -> BitcoindClient GetBlockResponse

-- | Returns the height of the most-work fully-validated chain.  The genesis block has height 0.
getBlockCount :: BitcoindClient Word32

-- | Returns hash of block in best-block-chain at height provided.
getBlockHash :: BlockHeight -> BitcoindClient BlockHash

-- | Retrieve a BIP 157 content filter for a particular block.
getBlockFilter :: BlockHash -> BitcoindClient CompactFilter

-- | Returns the header of the block corresponding to the given 'BlockHash'
getBlockHeader :: BlockHash -> BitcoindClient BlockHeader

{- | Compute per block statistics for a given window. All amounts are in
 satoshis.  It won't work for some heights with pruning.
-}
getBlockStats :: BlockHash -> Maybe [BlockStat] -> BitcoindClient BlockStats

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
    :<|> getBlock
    :<|> getBlockCount
    :<|> getBlockFilter
    :<|> getBlockHash
    :<|> getBlockHeader
    :<|> getBlockStats
    :<|> getChainTips
    :<|> getChainTxStats
    :<|> getDifficulty
    :<|> getMempoolAncestors
    :<|> getMempoolDescendants
    :<|> getMempoolInfo
    :<|> getRawMempool =
        toBitcoindClient $ Proxy @BlockchainRpc

{- | Produce the block corresponding to the given 'BlockHash' if it exists. Note
that this won't work for the Genesis block since to construct a 'BlockHeader' a
hash to a previous block is necessary.
-}
getBlockBlock :: GetBlockResponse -> Block
getBlockBlock = getBlockResponse id responseToBlock
  where
    responseToBlock response =
        Block
            ( H.BlockHeader
                { H.blockVersion = getBlockV2Version response
                , H.merkleRoot = getBlockV2MerkleRoot response
                , H.blockBits = getBlockV2Bits response
                , H.bhNonce = getBlockV2Nonce response
                , H.blockTimestamp = round . utcTimeToPOSIXSeconds $ getBlockV2Time response
                , H.prevBlock =
                    fromMaybe (error "no previous block hash on genesis") $
                        getBlockV2PreviousBlockHash response
                }
            )
            ((\(_, tx, _) -> tx) <$> getBlockV2Txs response)
