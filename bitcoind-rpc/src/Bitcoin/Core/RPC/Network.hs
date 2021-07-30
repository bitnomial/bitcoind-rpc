{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Bitcoin.Core.RPC.Network (
    Command (..),
    addNode,
    clearBanned,
    disconnectNode,
    NodeInfo (..),
    NodeInfoAddress (..),
    ConnDir (..),
    getAddedNodeInfo,
    getConnectionCount,
    NetTotals (..),
    getNetTotals,
    NodeAddress (..),
    getNodeAddresses,
    PeerInfo (..),
    getPeerInfo,
    listBanned,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    withObject,
    withText,
    (.:),
 )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word16, Word32, Word64)
import Haskoin.Block (BlockHeight)
import Servant.API ((:<|>) (..))

import Data.Aeson.Utils (toSatoshis, utcTime)
import Servant.Bitcoind (
    BitcoindClient,
    BitcoindEndpoint,
    C,
    CX,
    I,
    O,
    toBitcoindClient,
 )

-- | Commands as understood by 'addNode'
data Command = Add | Remove | OneTry deriving (Eq, Show, Enum)

commandText :: Command -> Text
commandText = \case
    Add -> "add"
    Remove -> "remove"
    OneTry -> "onetry"

instance ToJSON Command where
    toJSON = toJSON . commandText

data NodeAddress = NodeAddress
    { addrTime :: UTCTime
    , addrServices :: Word64
    , addrHost :: Text
    , addrPort :: Word32
    }
    deriving (Eq, Show)

instance FromJSON NodeAddress where
    parseJSON = withObject "NodeAddress" $ \o ->
        NodeAddress
            <$> (utcTime <$> o .: "time")
            <*> o .: "services"
            <*> o .: "address"
            <*> o .: "port"

data ConnDir = Inbound | Outbound deriving (Eq, Show, Enum)

instance FromJSON ConnDir where
    parseJSON = withText "ConnDir" fromText
      where
        fromText t
            | t == "inbound" = return Inbound
            | t == "outbound" = return Outbound
            | otherwise = fail "Unable to decode connection direction"

data NodeInfoAddress = NodeInfoAddress
    { nodeInfoAddress :: Text
    , connDirection :: ConnDir
    }
    deriving (Eq, Show)

instance FromJSON NodeInfoAddress where
    parseJSON = withObject "NodeInfoAddress" $ \o ->
        NodeInfoAddress <$> o .: "address" <*> o .: "connected"

data NodeInfo = NodeInfo
    { addedNode :: Text
    , connected :: Bool
    , addresses :: [NodeInfoAddress]
    }
    deriving (Eq, Show)

instance FromJSON NodeInfo where
    parseJSON = withObject "NodeInfo" $ \o ->
        NodeInfo <$> o .: "addednode" <*> o .: "connected" <*> o .: "addresses"

data NetTotals = NetTotals
    { bytesReceived :: Word64
    , bytesSent :: Word64
    }
    deriving (Eq, Show)

instance FromJSON NetTotals where
    parseJSON = withObject "NetTotals" $ \o ->
        NetTotals <$> o .: "totalbytesrecv" <*> o .: "totalbytessent"

data PeerInfo = PeerInfo
    { peerIndex :: Word16
    , peerAddr :: Text
    , peerBind :: Text
    , services :: Text
    , relay :: Bool
    , lastSend :: UTCTime
    , lastRecv :: UTCTime
    , peerBytesSent :: Word64
    , peerBytesRecv :: Word64
    , connTime :: UTCTime
    , timeOffset :: NominalDiffTime
    , pingTime :: Maybe Double
    , version :: Word64
    , inbound :: Bool
    , addnode :: Bool
    , startingHeight :: BlockHeight
    , banScore :: Word16
    , syncedHeaders :: Word32
    , syncedBlocks :: Word32
    , inflight :: [BlockHeight]
    , whitelisted :: Bool
    , -- | in satoshis
      minFeeFilter :: Word64
    }
    deriving (Eq, Show)

instance FromJSON PeerInfo where
    parseJSON = withObject "PeerInfo" $ \o ->
        PeerInfo
            <$> o .: "id"
            <*> o .: "addr"
            <*> o .: "addrbind"
            <*> o .: "services"
            <*> o .: "relaytxes"
            <*> (utcTime <$> o .: "lastsend")
            <*> (utcTime <$> o .: "lastrecv")
            <*> o .: "bytessent"
            <*> o .: "bytesrecv"
            <*> (utcTime <$> o .: "conntime")
            <*> (fromIntegral @Int <$> o .: "timeoffset")
            <*> o .: "pingtime"
            <*> o .: "version"
            <*> o .: "inbound"
            <*> o .: "addnode"
            <*> o .: "startingheight"
            <*> o .: "banscore"
            <*> o .: "synced_headers"
            <*> o .: "synced_blocks"
            <*> o .: "inflight"
            <*> o .: "whitelisted"
            <*> (toSatoshis <$> o .: "minfeefilter")

type NetworkRpc =
    BitcoindEndpoint "addnode" (I Text -> I Command -> CX)
        :<|> BitcoindEndpoint "clearbanned" CX
        :<|> BitcoindEndpoint "disconnectnode" (I Text -> CX)
        :<|> BitcoindEndpoint "getaddednodeinfo" (O Text -> C [NodeInfo])
        :<|> BitcoindEndpoint "getconnectioncount" (C Word16)
        :<|> BitcoindEndpoint "getnettotals" (C NetTotals)
        :<|> BitcoindEndpoint "getnodeaddresses" (O Word32 -> C [NodeAddress])
        :<|> BitcoindEndpoint "getpeerinfo" (C [PeerInfo])
        :<|> BitcoindEndpoint "listbanned" (C [Text])

{- | Attempts to add or remove a node from the addnode list; or try a
 connection to a node once.  Nodes added using addnode are protected from DoS
 disconnection and are not required to be full nodes/support SegWit as other
 outbound peers are (though such peers will not be synced from).
-}
addNode ::
    -- | node address @host:port@
    Text ->
    Command ->
    BitcoindClient ()

-- | Clear all banned IPs.
clearBanned :: BitcoindClient ()

-- | Immediately disconnects from the specified peer node.
disconnectNode ::
    -- | node address @host:port@
    Text ->
    BitcoindClient ()

{- | Returns information about the given added node, or all added nodes (note
 that onetry addnodes are not listed here)
-}
getAddedNodeInfo ::
    -- | optionally specify a node by address
    Maybe Text ->
    BitcoindClient [NodeInfo]

-- | Returns the number of connections to other nodes.
getConnectionCount :: BitcoindClient Word16

{- | Returns information about network traffic, including bytes in, bytes out,
 and current time.
-}
getNetTotals :: BitcoindClient NetTotals

{- | Return known addresses which can potentially be used to find new nodes in
 the network
-}
getNodeAddresses :: Maybe Word32 -> BitcoindClient [NodeAddress]

-- | Returns data about each connected network node.
getPeerInfo :: BitcoindClient [PeerInfo]

-- | List all banned IPs/Subnets.
listBanned :: BitcoindClient [Text]
addNode
    :<|> clearBanned
    :<|> disconnectNode
    :<|> getAddedNodeInfo
    :<|> getConnectionCount
    :<|> getNetTotals
    :<|> getNodeAddresses
    :<|> getPeerInfo
    :<|> listBanned =
        toBitcoindClient $ Proxy @NetworkRpc
