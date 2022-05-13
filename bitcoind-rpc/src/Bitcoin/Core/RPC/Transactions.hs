{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Bitcoin.Core.RPC.Transactions (
    getRawTransaction,
    sendRawTransaction,
    sendTransaction,
    MempoolTestResult (..),
    testMempoolAccept,
    FeeEstimationMode (..),
    EstimateSmartFeeResponse (..),
    estimateSmartFee,

    -- * PSBT
    PsbtMissing (..),
    AnalyzePsbtInput (..),
    AnalyzePsbtResponse (..),
    analyzePsbt,
    PsbtInput (..),
    PsbtOutputs (..),
    createPsbt,
    FinalizePsbtResponse (..),
    finalizePsbt,
    joinPsbts,
    Descriptor (..),
    utxoUpdatePsbt,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (toJSON),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.Aeson.Key as K
import Data.Aeson.Utils (
    Base64Encoded,
    HexEncoded (HexEncoded, unHexEncoded),
    partialObject,
    rangeToJSON,
    satsPerBTC,
    satsToBTCText,
    toSatoshis,
    unBase64Encoded,
    (.=?),
 )
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import Haskoin.Block (BlockHash, BlockHeight)
import Haskoin.Transaction (PartiallySignedTransaction, Tx, TxHash)
import Haskoin.Util (encodeHex)
import Servant.API ((:<|>) (..))
import Servant.Bitcoind (
    BitcoindClient,
    BitcoindEndpoint,
    C,
    DefFalse,
    F,
    I,
    O,
    toBitcoindClient,
 )

data MempoolTestResult = MempoolTestResult
    { testTxid :: TxHash
    , txAccepted :: Bool
    , rejectReason :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON MempoolTestResult where
    parseJSON = withObject "MempoolTestResult" $ \o ->
        MempoolTestResult <$> o .: "txid" <*> o .: "allowed" <*> o .:? "reject-reason"

type RawTxRpc =
    BitcoindEndpoint "sendrawtransaction" (I Text -> O Double -> C TxHash)
        :<|> BitcoindEndpoint "getrawtransaction" (I TxHash -> F DefFalse Bool -> O BlockHash -> C (HexEncoded Tx))
        :<|> BitcoindEndpoint "testmempoolaccept" (I [HexEncoded Tx] -> O Double -> C [MempoolTestResult])
        :<|> BitcoindEndpoint "analyzepsbt" (I Text -> C AnalyzePsbtResponse)
        :<|> BitcoindEndpoint
                "createpsbt"
                ( I [PsbtInput] ->
                  I PsbtOutputs ->
                  O Int ->
                  O Bool ->
                  C (Base64Encoded PartiallySignedTransaction)
                )
        :<|> BitcoindEndpoint
                "finalizepsbt"
                ( I Text ->
                  O Bool ->
                  C FinalizePsbtResponse
                )
        :<|> BitcoindEndpoint "joinpsbts" (I [Text] -> C (Base64Encoded PartiallySignedTransaction))
        :<|> BitcoindEndpoint "utxoupdatepsbt" (I Text -> I [Descriptor] -> C (Base64Encoded PartiallySignedTransaction))
        :<|> BitcoindEndpoint "estimatesmartfee" (I Int -> O FeeEstimationMode -> C EstimateSmartFeeResponse)

sendRawTransaction
    :<|> getRawTransaction'
    :<|> testMempoolAccept'
    :<|> analyzePsbt
    :<|> createPsbt_
    :<|> finalizePsbt
    :<|> joinPsbts_
    :<|> utxoUpdatePsbt_
    :<|> estimateSmartFee =
        toBitcoindClient $ Proxy @RawTxRpc

-- | Submit a raw transaction (serialized, hex-encoded) to local node and network.
sendRawTransaction :: Text -> Maybe Double -> BitcoindClient TxHash

-- | A version of 'sendRawTransaction' that handles serialization
sendTransaction :: Tx -> Maybe Double -> BitcoindClient TxHash
sendTransaction = sendRawTransaction . encodeHex . S.encode

getRawTransaction' :: TxHash -> Maybe BlockHash -> BitcoindClient (HexEncoded Tx)

{- | Returns result of mempool acceptance tests indicating if the transactions
 would be accepted by mempool.  This checks if the transaction violates the
 consensus or policy rules.
-}
testMempoolAccept :: [Tx] -> Maybe Double -> BitcoindClient [MempoolTestResult]
testMempoolAccept = testMempoolAccept' . fmap HexEncoded

testMempoolAccept' ::
    [HexEncoded Tx] ->
    Maybe Double ->
    BitcoindClient [MempoolTestResult]

{- | By default this function only works for mempool transactions. When called
 with a blockhash argument, getrawtransaction will return the transaction if
 the specified block is available and the transaction is found in that block.
 When called without a blockhash argument, getrawtransaction will return the
 transaction if it is in the mempool, or if -txindex is enabled and the
 transaction is in a block in the blockchain.
-}
getRawTransaction :: TxHash -> Maybe BlockHash -> BitcoindClient Tx
getRawTransaction h = fmap unHexEncoded . getRawTransaction' h

-- | @since 0.3.0.0
data PsbtMissing = PsbtMissing
    { missingPubkeys :: [Text]
    -- ^ Public key ID, hash160 of the public key, of a public key whose BIP 32 derivation path is missing
    , missingSigs :: [Text]
    -- ^ Public key ID, hash160 of the public key, of a public key whose signature is missing
    , redeemScript :: Maybe Text
    -- ^ Hash160 of the redeemScript that is missing
    , witnessScript :: Maybe Text
    -- ^ SHA256 of the witnessScript that is missing
    }
    deriving (Eq, Show)

instance FromJSON PsbtMissing where
    parseJSON = withObject "PsbtMissing" $ \obj ->
        PsbtMissing
            <$> (fromMaybe mempty <$> obj .:? "pubkeys")
            <*> obj .: "signatures"
            <*> obj .:? "redeemscript"
            <*> obj .:? "witnessscript"

-- | @since 0.3.0.0
data AnalyzePsbtInput = AnalyzePsbtInput
    { hasUtxo :: Bool
    -- ^ Whether a UTXO is provided
    , isFinal :: Bool
    -- ^ Whether the input is finalized
    , missing :: Maybe PsbtMissing
    , next :: Maybe Text
    -- ^ Role of the next person that this input needs to go to
    }
    deriving (Eq, Show)

instance FromJSON AnalyzePsbtInput where
    parseJSON = withObject "AnalyzePsbtInput" $ \obj ->
        AnalyzePsbtInput
            <$> obj .: "has_utxo"
            <*> obj .: "is_final"
            <*> obj .:? "missing"
            <*> obj .:? "next"

-- | @since 0.3.0.0
data AnalyzePsbtResponse = AnalyzePsbtResponse
    { analysePsbtInput :: [AnalyzePsbtInput]
    , analyzePsbtEstimatedVSize :: Maybe Int
    -- ^ Estimated vsize of the final signed transaction
    , analyzePsbtEstimatedFeeRate :: Maybe Scientific
    -- ^ Estimated feerate of the final signed transaction in sats/kB. Shown only if all UTXO slots in the PSBT have been filled.
    , analyzePsbtFee :: Maybe Word64
    -- ^ The transaction fee paid. Shown only if all UTXO slots in the PSBT have been filled.
    , analyzePsbtNext :: Maybe Text
    -- ^ Role of the next person that this psbt needs to go to
    , analyzePsbtError :: Maybe Text
    -- ^ Error message if there is one
    }
    deriving (Eq, Show)

instance FromJSON AnalyzePsbtResponse where
    parseJSON = withObject "AnalyzePsbtResponse" $ \obj ->
        AnalyzePsbtResponse
            <$> obj .: "inputs"
            <*> obj .:? "estimated_vsize"
            <*> (fmap (* satsPerBTC) <$> obj .:? "estimated_feerate")
            <*> (fmap toSatoshis <$> obj .:? "fee")
            <*> obj .:? "next"
            <*> obj .:? "error"

{- | Analyzes and provides information about the current status of a PSBT and its inputs

  @since 0.3.0.0
-}
analyzePsbt ::
    -- | A base64 string of a PSBT
    Text ->
    BitcoindClient AnalyzePsbtResponse

-- | @since 0.3.0.0
data PsbtInput = PsbtInput
    { psbtInputTx :: TxHash
    -- ^ The transaction id
    , psbtInputVOut :: Word32
    -- ^ The output number
    , psbtInputSequence :: Maybe Int
    -- ^ The sequence number
    }
    deriving (Eq, Show)

instance ToJSON PsbtInput where
    toJSON input =
        partialObject
            [ Just $ "txid" .= psbtInputTx input
            , Just $ "vout" .= psbtInputVOut input
            , "sequence" .=? psbtInputSequence input
            ]

-- | @since 0.3.0.0
data PsbtOutputs = PsbtOutputs
    { psbtOutputAddrs :: [(Text, Word64)]
    , psbtOutputData :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON PsbtOutputs where
    toJSON outputs =
        toJSON $
            (fmap toAddrObject . psbtOutputAddrs) outputs
                <> (foldMap toDataObject . psbtOutputData) outputs
      where
        toAddrObject (addr, amount) = object [K.fromText addr .= satsToBTCText amount]
        toDataObject hex = [object ["data" .= hex]]

{- | Creates a transaction in the Partially Signed Transaction format. Implements the Creator role.

 @since 0.3.0.0
-}
createPsbt ::
    [PsbtInput] ->
    PsbtOutputs ->
    -- | Raw locktime. Non-0 value also locktime-activates inputs
    Maybe Int ->
    -- | Marks this transaction as BIP125 replaceable.
    Maybe Bool ->
    BitcoindClient PartiallySignedTransaction
createPsbt inputs outputs locktime = fmap unBase64Encoded . createPsbt_ inputs outputs locktime

createPsbt_ ::
    [PsbtInput] ->
    PsbtOutputs ->
    Maybe Int ->
    Maybe Bool ->
    BitcoindClient (Base64Encoded PartiallySignedTransaction)

-- | @since 0.3.0.0
data FinalizePsbtResponse = FinalizePsbtResponse
    { finalizedPsbt :: Maybe PartiallySignedTransaction
    -- ^ The base64-encoded partially signed transaction if not extracted
    , finalizedTx :: Maybe Tx
    -- ^ The hex-encoded network transaction if extracted
    , finalizeComplete :: Bool
    -- ^ If the transaction has a complete set of signatures
    }
    deriving (Eq, Show)

instance FromJSON FinalizePsbtResponse where
    parseJSON = withObject "FinalizePsbtResponse" $ \obj ->
        FinalizePsbtResponse
            <$> (fmap unBase64Encoded <$> obj .:? "psbt")
            <*> (fmap unHexEncoded <$> obj .:? "hex")
            <*> obj .: "complete"

{- | Finalize the inputs of a PSBT. If the transaction is fully signed, it will
 produce a network serialized transaction which can be broadcast with
 sendrawtransaction. Otherwise a PSBT will be created which has the
 final_scriptSig and final_scriptWitness fields filled for inputs that are
 complete.  Implements the Finalizer and Extractor roles.

 @since 0.3.0.0
-}
finalizePsbt ::
    -- | A base64 string of a PSBT
    Text ->
    -- | If true and the transaction is complete, extract and return the
    -- complete transaction in normal network serialization instead of the PSBT.
    Maybe Bool ->
    BitcoindClient FinalizePsbtResponse

{- | Joins multiple distinct PSBTs with different inputs and outputs into one
 PSBT with inputs and outputs from all of the PSBTs No input in any of the PSBTs
 can be in more than one of the PSBTs.

 @since 0.3.0.0
-}
joinPsbts ::
    -- | A base64 string of a PSBT
    [Text] ->
    BitcoindClient PartiallySignedTransaction
joinPsbts = fmap unBase64Encoded . joinPsbts_

joinPsbts_ ::
    [Text] ->
    BitcoindClient (Base64Encoded PartiallySignedTransaction)

-- | @since 0.3.0.0
data Descriptor
    = Descriptor Text
    | RangedDescriptor Text (Int, Maybe Int)
    deriving (Eq, Show)

instance ToJSON Descriptor where
    toJSON = \case
        Descriptor theDescriptor -> toJSON theDescriptor
        RangedDescriptor theDescriptor range ->
            object
                [ "desc" .= theDescriptor
                , "range" .= rangeToJSON range
                ]

{- | Updates all segwit inputs and outputs in a PSBT with data from output
 descriptors, the UTXO set or the mempool.

 @since 0.3.0.0
-}
utxoUpdatePsbt ::
    -- | A base64 string of a PSBT
    Text ->
    [Descriptor] ->
    BitcoindClient PartiallySignedTransaction
utxoUpdatePsbt psbt = fmap unBase64Encoded . utxoUpdatePsbt_ psbt

utxoUpdatePsbt_ ::
    Text ->
    [Descriptor] ->
    BitcoindClient (Base64Encoded PartiallySignedTransaction)

-- | @since 0.3.0.0
data FeeEstimationMode
    = Conservative
    | Economical
    | Unset
    deriving (Eq, Show)

instance FromJSON FeeEstimationMode where
    parseJSON = withText "FeeEstimationMode" $ \case
        "conservative" -> pure Conservative
        "economical" -> pure Economical
        "unset" -> pure Unset
        other -> fail $ "Unknown fee estimation mode: " <> Text.unpack other

instance ToJSON FeeEstimationMode where
    toJSON = \case
        Conservative -> "conservative"
        Economical -> "economical"
        Unset -> "unset"

-- | @since 0.3.0.0
data EstimateSmartFeeResponse = EstimateSmartFeeResponse
    { estimateSmartFeeFee :: Maybe Word64
    -- ^ estimate fee rate in sats/vB (only present if no errors were encountered)
    , estimateSmartFeeErrors :: [Text]
    , estimateSmartFeeBolcks :: BlockHeight
    -- ^ block number where estimate was found.  The request target will be
    -- clamped between 2 and the highest target fee estimation is able to return based
    -- on how long it has been running. An error is returned if not enough transactions
    -- and blocks have been observed to make an estimate for any number of blocks.
    }
    deriving (Eq, Show)

instance FromJSON EstimateSmartFeeResponse where
    parseJSON = withObject "EstimateSmartFeeResponse" $ \obj -> do
        EstimateSmartFeeResponse
            <$> (fmap changeUnits <$> obj .:? "feerate")
            <*> (fromMaybe mempty <$> obj .:? "errors")
            <*> obj .: "blocks"
      where
        -- BTC/kvb -> sats/vb
        changeUnits = toSatoshis . (/ 1_000)

{- | Estimates the approximate fee per kilobyte needed for a transaction to
begin confirmation within conf_target blocks if possible and return the number
of blocks for which the estimate is valid. Uses virtual transaction size as
defined in BIP 141 (witness data is discounted).

@since 0.3.0.0
-}
estimateSmartFee ::
    -- | Confirmation target in blocks (1 - 1008)
    Int ->
    Maybe FeeEstimationMode ->
    BitcoindClient EstimateSmartFeeResponse
