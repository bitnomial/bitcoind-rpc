{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- |
 Module: Bitcoin.Core.RPC.Wallet

 Wallet related functionality
-}
module Bitcoin.Core.RPC.Wallet (
    abandonTransaction,
    abortRescan,
    AddressType (..),
    NewMultisigAddress (..),
    addMultisigAddress,
    backupWallet,
    BumpFeeOptions (..),
    BumpFeeResponse (..),
    bumpFee,
    LoadWalletResponse (..),
    createWallet,
    dumpPrivKey,
    dumpWallet,
    encryptWallet,
    Purpose (..),
    getAddressesByLabel,
    AddressInfo (..),
    getAddressInfo,
    getBalance,
    BalanceDetails (..),
    Balances (..),
    getBalances,
    getNewAddress,
    getRawChangeAddress,
    getReceivedByAddress,
    getReceivedByLabel,
    Category (..),
    TransactionDetails (..),
    GetTransactionResponse (..),
    getTransaction,
    WalletStateInfo (..),
    getWalletInfo,
    importAddress,
    DescriptorRequest (..),
    ImportResponse (..),
    importDescriptors,
    ImportMultiRequest (..),
    importMulti,
    importPrivKey,
    importWallet,
    DescriptorDetails (..),
    listDescriptors,
    listLabels,
    listLockUnspent,
    ListReceivedResponse (..),
    listReceivedByAddress,
    ListReceivedByLabelResponse (..),
    listReceivedByLabel,
    ListSinceBlockResponse (..),
    listSinceBlock,
    listTransactions,
    ListUnspentOptions (..),
    OutputDetails (..),
    listUnspent,
    listWallets,
    loadWallet,
    lockUnspent,
    psbtBumpFee,
    RescanResponse (..),
    rescanBlockchain,
    FeeEstimationMode (..),
    sendMany,
    sendToAddress,
    setLabel,
    setTxFee,
    signMessage,
    PrevTx (..),
    SignRawTxResponse (..),
    signRawTx,
    unloadWallet,
    CreatePsbtOptions (..),
    CreatePsbtResponse (..),
    createFundedPsbt,
    walletLock,
    walletPassphrase,
    ProcessPsbtResponse (..),
    processPsbt,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (Object),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word32, Word64)
import Haskoin (
    BlockHash,
    BlockHeight,
    DerivPath,
    Hash160,
    OutPoint (OutPoint),
    PartiallySignedTransaction,
    Tx,
    TxHash,
 )
import Servant.API ((:<|>) (..))

import Bitcoin.Core.RPC.Transactions (PsbtInput, PsbtOutputs)
import Data.Aeson.Utils (
    partialObject,
    rangeToJSON,
    satsToBTCText,
    toSatoshis,
    unBase64Encoded,
    unHexEncoded,
    utcTime,
    (.=?),
 )
import Servant.Bitcoind (
    BitcoindClient,
    BitcoindEndpoint,
    C,
    CX,
    DefFalse,
    DefZero,
    EmptyString,
    F,
    I,
    O,
    toBitcoindClient,
 )

type WalletRpc =
    BitcoindEndpoint "abandontransaction" (I TxHash -> CX)
        :<|> BitcoindEndpoint "abortrescan" (C Bool)
        :<|> BitcoindEndpoint
                "addmultisigaddress"
                ( I Int ->
                  I [Text] ->
                  O Text ->
                  O AddressType ->
                  C NewMultisigAddress
                )
        :<|> BitcoindEndpoint "backupwallet" (I FilePath -> CX)
        :<|> BitcoindEndpoint "bumpfee" (I TxHash -> O BumpFeeOptions -> C BumpFeeResponse)
        :<|> BitcoindEndpoint
                "createwallet"
                ( I Text ->
                  O Bool ->
                  O Bool ->
                  I Text ->
                  O Bool ->
                  O Bool ->
                  O Bool ->
                  O Bool ->
                  C LoadWalletResponse
                )
        :<|> BitcoindEndpoint "dumpprivkey" (I Text -> C Text)
        :<|> BitcoindEndpoint "dumpwallet" (I FilePath -> C DumpWalletResponse)
        :<|> BitcoindEndpoint "encryptwallet" (I Text -> C Text)
        :<|> BitcoindEndpoint "getaddressesbylabel" (I Text -> C (Map Text Purpose))
        :<|> BitcoindEndpoint "getaddressinfo" (I Text -> C AddressInfo)
        :<|> BitcoindEndpoint "getbalance" (O Text -> O Int -> O Bool -> O Bool -> C Scientific)
        :<|> BitcoindEndpoint "getbalances" (C Balances)
        :<|> BitcoindEndpoint "getnewaddress" (O Text -> O AddressType -> C Text)
        :<|> BitcoindEndpoint "getrawchangeaddress" (O AddressType -> C Text)
        :<|> BitcoindEndpoint "getreceivedbyaddress" (I Text -> O Int -> C Scientific)
        :<|> BitcoindEndpoint "getreceivedbylabel" (I Text -> O Int -> C Scientific)
        :<|> BitcoindEndpoint
                "gettransaction"
                ( I TxHash ->
                  O Bool ->
                  F DefFalse Bool ->
                  C GetTransactionResponse
                )
        :<|> BitcoindEndpoint "getwalletinfo" (C WalletStateInfo)
        :<|> BitcoindEndpoint "importaddress" (I Text -> O Text -> O Bool -> O Bool -> CX)
        :<|> BitcoindEndpoint "importdescriptors" (I [DescriptorRequest] -> C [ImportResponse])
        :<|> BitcoindEndpoint "importmulti" (I [ImportMultiRequest] -> O ImportMultiOptions -> C [ImportResponse])
        :<|> BitcoindEndpoint "importprivkey" (I Text -> O Text -> O Bool -> CX)
        -- WAIT importprunedfunds "rawtransaction" "txoutproof"
        --      :<|> BitcoindEndpoint "importprunedfunds" ()
        -- WAIT importpubkey "pubkey" ( "label" rescan )
        --      :<|> BitcoindEndpoint "importpubkey" ()
        :<|> BitcoindEndpoint "importwallet" (I FilePath -> CX)
        -- WAIT keypoolrefill ( newsize )
        --      :<|> BitcoindEndpoint "keypoolrefill" ()
        -- WAIT listaddressgroupings
        --      :<|> BitcoindEndpoint "listaddressgroupings" ()
        :<|> BitcoindEndpoint "listdescriptors" (C [DescriptorDetails])
        :<|> BitcoindEndpoint "listlabels" (O Text -> C [Text])
        :<|> BitcoindEndpoint "listlockunspent" (C [JsonOutPoint])
        :<|> BitcoindEndpoint
                "listreceivedbyaddress"
                ( O Int ->
                  O Bool ->
                  O Bool ->
                  O Text ->
                  C [ListReceivedResponse]
                )
        :<|> BitcoindEndpoint
                "listreceivedbylabel"
                ( O Int ->
                  O Bool ->
                  O Bool ->
                  C [ListReceivedByLabelResponse]
                )
        :<|> BitcoindEndpoint
                "listsinceblock"
                ( O BlockHash ->
                  O Int ->
                  O Bool ->
                  O Bool ->
                  C ListSinceBlockResponse
                )
        :<|> BitcoindEndpoint
                "listtransactions"
                ( O Text ->
                  O Int ->
                  O Int ->
                  O Bool ->
                  C [TransactionDetails]
                )
        :<|> BitcoindEndpoint
                "listunspent"
                ( O Int ->
                  O Int ->
                  O [Text] ->
                  O Bool ->
                  I ListUnspentOptions ->
                  C [OutputDetails]
                )
        -- WAIT listwalletdir
        --      :<|> BitcoindEndpoint "listwalletdir" ()
        :<|> BitcoindEndpoint "listwallets" (C [Text])
        :<|> BitcoindEndpoint "loadwallet" (I Text -> O Bool -> C LoadWalletResponse)
        :<|> BitcoindEndpoint "lockunspent" (I Bool -> I [PrevTx] -> C Bool)
        :<|> BitcoindEndpoint "psbtbumpfee" (I TxHash -> O BumpFeeOptions -> C BumpFeeResponse)
        -- WAIT removeprunedfunds "txid"
        --      :<|> BitcoindEndpoint "removeprunedfunds" ()
        :<|> BitcoindEndpoint
                "rescanblockchain"
                ( O BlockHeight ->
                  O BlockHeight ->
                  C RescanResponse
                )
        -- WAIT send [{"address":amount},{"data":"hex"},...] ( conf_target "estimate_mode" fee_rate options )
        --      :<|> BitcoindEndpoint "send" ()
        :<|> BitcoindEndpoint
                "sendmany"
                ( F EmptyString Text ->
                  I (Map Text Text) ->
                  F DefZero Int ->
                  O Text ->
                  I [Text] ->
                  O Bool ->
                  O Int ->
                  O FeeEstimationMode ->
                  O Word64 ->
                  F DefFalse Bool ->
                  C TxHash
                )
        :<|> BitcoindEndpoint
                "sendtoaddress"
                ( I Text ->
                  I Text ->
                  O Text ->
                  O Text ->
                  O Bool ->
                  O Bool ->
                  O Int ->
                  O FeeEstimationMode ->
                  O Bool ->
                  O Word64 ->
                  F DefFalse Bool ->
                  C TxHash
                )
        -- WAIT sethdseed ( newkeypool "seed" )
        --      :<|> BitcoindEndpoint "sethdseed" ()
        :<|> BitcoindEndpoint "setlabel" (I Text -> I Text -> CX)
        :<|> BitcoindEndpoint "settxfee" (I Text -> C Bool)
        -- WAIT setwalletflag "flag" ( value )
        --      :<|> BitcoindEndpoint "setwalletflag" ()
        :<|> BitcoindEndpoint "signmessage" (I Text -> I Text -> C Text)
        :<|> BitcoindEndpoint
                "signrawtransactionwithwallet"
                ( I Text ->
                  I [PrevTx] ->
                  O Text ->
                  C SignRawTxResponse
                )
        :<|> BitcoindEndpoint "unloadwallet" (O Text -> O Bool -> C UnloadWalletResponse)
        -- WAIT upgradewallet ( version )
        --      :<|> BitcoindEndpoint "upgradewallet" ()
        :<|> BitcoindEndpoint
                "walletcreatefundedpsbt"
                ( I [PsbtInput] ->
                  I PsbtOutputs ->
                  O Int ->
                  O CreatePsbtOptions ->
                  O Bool ->
                  C CreatePsbtResponse
                )
        -- WAIT walletlock
        :<|> BitcoindEndpoint "walletlock" CX
        :<|> BitcoindEndpoint "walletpassphrase" (I Text -> I Int -> CX)
        -- WAIT walletpassphrasechange "oldpassphrase" "newpassphrase"
        --      :<|> BitcoindEndpoint "walletpassphrasechange" ()
        :<|> BitcoindEndpoint
                "walletprocesspsbt"
                ( I Text ->
                  O Bool ->
                  I Text ->
                  O Bool ->
                  C ProcessPsbtResponse
                )

-- client definition
abandonTransaction
    :<|> abortRescan
    :<|> addMultisigAddress
    :<|> backupWallet
    :<|> bumpFee
    :<|> createWallet
    :<|> dumpPrivKey
    :<|> dumpWallet_
    :<|> encryptWallet
    :<|> getAddressesByLabel
    :<|> getAddressInfo
    :<|> getBalance_
    :<|> getBalances
    :<|> getNewAddress
    :<|> getRawChangeAddress
    :<|> getReceivedByAddress_
    :<|> getReceivedByLabel_
    :<|> getTransaction
    :<|> getWalletInfo
    :<|> importAddress
    :<|> importDescriptors
    :<|> importMulti_
    :<|> importPrivKey
    :<|> importWallet
    :<|> listDescriptors
    :<|> listLabels_
    :<|> listLockUnspent_
    :<|> listReceivedByAddress
    :<|> listReceivedByLabel
    :<|> listSinceBlock
    :<|> listTransactions
    :<|> listUnspent
    :<|> listWallets
    :<|> loadWallet
    :<|> lockUnspent
    :<|> psbtBumpFee
    :<|> rescanBlockchain
    :<|> sendMany_
    :<|> sendToAddress_
    :<|> setLabel
    :<|> setTxFee_
    :<|> signMessage
    :<|> signRawTx
    :<|> unloadWallet_
    :<|> createFundedPsbt
    :<|> walletLock
    :<|> walletPassphrase
    :<|> processPsbt_ =
        toBitcoindClient $ Proxy @WalletRpc

{- | Mark in-wallet transaction <txid> as abandoned This will mark this
 transaction and all its in-wallet descendants as abandoned which will allow for
 their inputs to be respent.  It can be used to replace "stuck" or evicted
 transactions.  It only works on transactions which are not included in a block
 and are not currently in the mempool.  It has no effect on transactions which
 are already abandoned.

 @since 0.3.0.0
-}
abandonTransaction ::
    -- | The transaction id
    TxHash ->
    BitcoindClient ()

{- | Stops current wallet rescan triggered by an RPC call, e.g. by an
 importprivkey call.  Note: Use "getwalletinfo" to query the scanning progress.

 @since 0.3.0.0
-}
abortRescan ::
    -- | Whether the abort was successful
    BitcoindClient Bool

-- | @since 0.3.0.0
data NewMultisigAddress = NewMultisigAddress
    { -- | The value of the new multisig address
      newMultisigAddress :: Text
    , -- | The string value of the hex-encoded redemption script
      newMultisigRedeemScript :: Text
    , -- | The descriptor for this multisig
      newMultisigDescriptor :: Text
    }
    deriving (Eq, Show)

instance FromJSON NewMultisigAddress where
    parseJSON = withObject "NewMultisigAddress" $ \obj ->
        NewMultisigAddress
            <$> obj .: "address"
            <*> obj .: "redeemScript"
            <*> obj .: "descriptor"

-- | @since 0.3.0.0
data AddressType = Legacy | P2SHSegwit | Bech32
    deriving (Eq, Show)

instance ToJSON AddressType where
    toJSON =
        toJSON @Text . \case
            Legacy -> "legacy"
            P2SHSegwit -> "p2sh-segwit"
            Bech32 -> "bech32"

{- | Add an nrequired-to-sign multisignature address to the wallet. Requires a
 new wallet backup.  Each key is a Bitcoin address or hex-encoded public key.
 This functionality is only intended for use with non-watchonly addresses.  See
 `importaddress` for watchonly p2sh address support.  If 'label' is specified,
 assign address to that label.

 @since 0.3.0.0
-}
addMultisigAddress ::
    -- | The number of required signatures out of the n keys or addresses.
    Int ->
    -- | The bitcoin addresses or hex-encoded public keys
    [Text] ->
    -- | A label to assign the addresses to.
    Maybe Text ->
    Maybe AddressType ->
    BitcoindClient NewMultisigAddress

{- | Safely copies current wallet file to destination, which can be a directory
 or a path with filename.

 @since 0.3.0.0
-}
backupWallet ::
    -- | The destination directory or file
    FilePath ->
    BitcoindClient ()

-- | @since 0.3.0.0
data BumpFeeOptions = BumpFeeOptions
    { -- | Confirmation target in blocks
      bumpFeeConfTarget :: Maybe Int
    , -- | Specify a fee rate in sat/vB instead of relying on the built-in fee estimator.
      -- Must be at least 1.000 sat/vB higher than the current transaction fee rate.
      bumpFeeFeeRate :: Maybe Scientific
    , -- | Whether the new transaction should still be
      -- marked bip-125 replaceable. If true, the sequence numbers in the transaction will
      -- be left unchanged from the original. If false, any input sequence numbers in the
      -- original transaction that were less than 0xfffffffe will be increased to 0xfffffffe
      -- so the new transaction will not be explicitly bip-125 replaceable (though it may
      -- still be replaceable in practice, for example if it has unconfirmed ancestors which
      -- are replaceable).
      bumpFeeReplaceable :: Bool
    , bumpFeeEstimateMode :: Maybe FeeEstimationMode
    }
    deriving (Eq, Show)

instance ToJSON BumpFeeOptions where
    toJSON opts =
        partialObject
            [ "conf_target" .=? bumpFeeConfTarget opts
            , "fee_rate" .=? bumpFeeFeeRate opts
            , Just $ "replaceable" .= bumpFeeReplaceable opts
            , "estimate_mode" .=? bumpFeeEstimateMode opts
            ]

-- | @since 0.3.0.0
data BumpFeeResponse = BumpFeeResponse
    { -- | The base64-encoded unsigned PSBT of the new transaction. (only available with 'psbtBumpFee')
      bumpFeePSBT :: Maybe PartiallySignedTransaction
    , -- | The id of the new transaction. Only returned when wallet private keys are enabled.
      bumpFeeTxId :: Maybe TxHash
    , -- | The fee of the replaced transaction.
      bumpFeeOrig :: Scientific
    , -- | The fee of the new transaction.
      bumpFeeFee :: Scientific
    , -- | Errors encountered during processing (may be empty).
      bumpFeeErrors :: [Text]
    }
    deriving (Eq, Show)

instance FromJSON BumpFeeResponse where
    parseJSON = withObject "BumpFeeResponse" $ \obj ->
        BumpFeeResponse
            <$> (fmap unBase64Encoded <$> obj .:? "psbt")
            <*> obj .:? "txid"
            <*> obj .: "origfee"
            <*> obj .: "fee"
            <*> obj .: "errors"

{- | Bumps the fee of an opt-in-RBF transaction T, replacing it with a new
 transaction B.  An opt-in RBF transaction with the given txid must be in the
 wallet.  The command will pay the additional fee by reducing change outputs or
 adding inputs when necessary.  It may add a new change output if one does not
 already exist.  All inputs in the original transaction will be included in the
 replacement transaction.  The command will fail if the wallet or mempool
 contains a transaction that spends one of T's outputs.  By default, the new fee
 will be calculated automatically using the estimatesmartfee RPC.  The user can
 specify a confirmation target for estimatesmartfee.  Alternatively, the user
 can specify a fee rate in sat/vB for the new transaction.  At a minimum, the
 new fee rate must be high enough to pay an additional new relay fee
 (incrementalfee returned by getnetworkinfo) to enter the node's mempool.

 Bitcoind >= 0.21

 @since 0.3.0.0
-}
bumpFee ::
    TxHash ->
    Maybe BumpFeeOptions ->
    BitcoindClient BumpFeeResponse

data LoadWalletResponse = LoadWalletResponse
    { -- | The wallet name if created successfully. If the wallet was created
      -- using a full path, the wallet_name will be the full path.
      loadWalletName :: Text
    , -- | Warning message if wallet was not loaded cleanly.
      loadWalletWarning :: Text
    }
    deriving (Eq, Show)

instance FromJSON LoadWalletResponse where
    parseJSON = withObject "LoadWalletResponse" $ \obj ->
        LoadWalletResponse
            <$> obj .: "name"
            <*> obj .: "warning"

-- | Creates and loads a new wallet.
createWallet ::
    -- | The name for the new wallet. If this is a path, the wallet will be
    -- created at the path location.
    Text ->
    -- | Disable the possibility of private keys (only watchonlys are possible
    -- in this mode).  (Default: False)
    Maybe Bool ->
    -- | Create a blank wallet. A blank wallet has no keys or HD seed. One can
    -- be set using sethdseed.  (Default: False)
    Maybe Bool ->
    -- | Encrypt the wallet with this passphrase.
    Text ->
    -- | Keep track of coin reuse, and treat dirty and clean coins differently
    -- with privacy considerations in mind.  (Default: False)
    Maybe Bool ->
    -- | Create a native descriptor wallet. The wallet will use descriptors
    -- internally to handle address creation.  (Default: False)
    Maybe Bool ->
    -- | Save wallet name to persistent settings and load on startup. True to
    -- add wallet to startup list, false to remove, null to leave unchanged.
    Maybe Bool ->
    -- | Use an external signer such as a hardware wallet. Requires -signer to
    -- be configured. Wallet creation will fail if keys cannot be fetched. Requires
    -- disable_private_keys and descriptors set to true.  (Default: False)
    Maybe Bool ->
    BitcoindClient LoadWalletResponse

{- | Reveals the private key corresponding to 'address'.  Then the importprivkey
 can be used with this output

 @since 0.3.0.0
-}
dumpPrivKey ::
    -- | The bitcoin address for the private key
    Text ->
    -- | The private key
    BitcoindClient Text

newtype DumpWalletResponse = DumpWalletResponse {dumpWalletFileName :: FilePath}
    deriving (Eq, Show)

instance FromJSON DumpWalletResponse where
    parseJSON = withObject "DumpWalletResponse" $ fmap DumpWalletResponse . (.: "filename")

dumpWallet_ :: FilePath -> BitcoindClient DumpWalletResponse

{- | Dumps all wallet keys in a human-readable format to a server-side file.
 This does not allow overwriting existing files.  Imported scripts are included
 in the dumpfile, but corresponding BIP173 addresses, etc. may not be added
 automatically by importwallet.  Note that if your wallet contains keys which
 are not derived from your HD seed (e.g. imported keys), these are not covered
 by only backing up the seed itself, and must be backed up too (e.g. ensure you
 back up the whole dumpfile).

 @since 0.3.0.0
-}
dumpWallet ::
    -- | The filename with path (absolute path recommended)
    FilePath ->
    -- | The filename with full absolute path
    BitcoindClient FilePath
dumpWallet = fmap dumpWalletFileName . dumpWallet_

{- | Encrypts the wallet with 'passphrase'. This is for first time encryption.
 After this, any calls that interact with private keys such as sending or signing
 will require the passphrase to be set prior the making these calls.
 Use the walletpassphrase call for this, and then walletlock call.
 If the wallet is already encrypted, use the walletpassphrasechange call.

 @since 0.3.0.0
-}
encryptWallet ::
    -- | The pass phrase to encrypt the wallet with. It must be at least 1
    -- character, but should be long.
    Text ->
    -- | A string with further instructions
    BitcoindClient Text

data Purpose = PurposeSend | PurposeRecv
    deriving (Eq, Show)

instance FromJSON Purpose where
    parseJSON = withObject "Purpose" $ \obj ->
        obj .: "purpose" >>= \case
            "send" -> pure PurposeSend
            "receive" -> pure PurposeRecv
            other -> fail $ "Unknown purpose: " <> other

purposeText :: Purpose -> Text
purposeText = \case
    PurposeSend -> "send"
    PurposeRecv -> "receive"

{- | Returns the list of addresses assigned the specified label.

 @since 0.3.0.0
-}
getAddressesByLabel ::
    -- | The label.
    Text ->
    -- | Keys: addresses, values: purpose of address ("send" for sending
    -- address, "receive" for receiving address)
    BitcoindClient (Map Text Purpose)

-- | @since 0.3.0.0
data AddressInfo = AddressInfo
    { -- | The bitcoin address validated.
      addressInfoAddress :: Text
    , -- | The hex-encoded scriptPubKey generated by the address.
      addressInfoScriptPubKey :: Text
    , -- | If the address is yours.
      addressInfoIsMine :: Bool
    , -- | If the address is watchonly.
      addressInfoWatchOnly :: Bool
    , -- | If we know how to spend coins sent to this address, ignoring the
      -- possible lack of private keys.
      addressInfoSolvable :: Bool
    , -- | A descriptor for spending coins sent to this address (only when solvable).
      addressInfoDescriptor :: Maybe Text
    , -- | The descriptor used to derive this address if this is a descriptor wallet
      addressInfoParentDesc :: Maybe Text
    , -- | If the key is a script.
      addressInfoIsScript :: Bool
    , -- | If the address was used for change output.
      addressInfoIsChange :: Bool
    , -- | If the address is a witness address.
      addressInfoIsWitness :: Bool
    , -- | The version number of the witness program.
      addressInfoWitnessVersion :: Maybe Int
    , -- | The hex value of the witness program.
      addressInfoWitnessProgram :: Maybe Text
    , -- | The output script type. Only if isscript is true and the redeemscript is known. Possible
      -- types: nonstandard, pubkey, pubkeyhash, scripthash, multisig, nulldata, witness_v0_keyhash,
      -- witness_v0_scripthash, witness_unknown.
      addressInfoScriptType :: Maybe Text
    , -- | The redeemscript for the p2sh address.
      addressInfoScriptHex :: Maybe Text
    , -- | Array of pubkeys associated with the known redeemscript (only if script is multisig).
      addressInfoPubkeys :: [Text]
    , -- | The number of signatures required to spend multisig output (only if script is multisig).
      addressInfoSigsRequired :: Maybe Int
    , -- | The hex value of the raw public key for single-key addresses (possibly embedded in P2SH or P2WSH).
      addressInfoPubkey :: Maybe Text
    , -- | Information about the address embedded in P2SH or P2WSH, if relevant and known.
      -- TODO precise type
      addressInfoEmbedded :: Maybe Value
    , -- | If the pubkey is compressed.
      addressInfoIsCompressed :: Maybe Bool
    , -- | The creation time of the key, if available, expressed in UNIX epoch time.
      addressInfoTimestamp :: Maybe UTCTime
    , -- | The HD keypath, if the key is HD and available.
      addressInfoHDKeyPath :: Maybe DerivPath
    , -- | The Hash160 of the HD seed.
      addressInfoHDSeedId :: Maybe Hash160
    , -- | The fingerprint of the master key.
      addressInfoHDMasterFingerprint :: Maybe Text
    , -- | Array of labels associated with the address. Currently limited to one label but returned
      -- as an array to keep the API stable if multiple labels are enabled in the future.
      addressInfoLabels :: [Text]
    }
    deriving (Eq, Show)

instance FromJSON AddressInfo where
    parseJSON = withObject "AddressInfo" $ \obj ->
        AddressInfo
            <$> obj .: "address"
            <*> obj .: "scriptPubKey"
            <*> obj .: "ismine"
            <*> obj .: "iswatchonly"
            <*> obj .: "solvable"
            <*> obj .:? "desc"
            <*> obj .:? "parent_desc"
            <*> obj .: "isscript"
            <*> obj .: "ischange"
            <*> obj .: "iswitness"
            <*> obj .:? "witness_version"
            <*> obj .:? "witness_program"
            <*> obj .:? "script"
            <*> obj .:? "hex"
            <*> (fromMaybe mempty <$> obj .:? "pubkeys")
            <*> obj .:? "sigsrequired"
            <*> obj .:? "pubkey"
            <*> obj .:? "embedded"
            <*> obj .:? "compressed"
            <*> (fmap utcTime <$> obj .:? "timestamp")
            <*> obj .:? "hdkeypath"
            <*> (fmap unHexEncoded <$> obj .:? "hdseedid")
            <*> obj .:? "hdmasterfingerprint"
            <*> obj .: "labels"

{- | Return information about the given bitcoin address.
 Some of the information will only be present if the address is in the active wallet.

 @since 0.3.0.0
-}
getAddressInfo ::
    -- | The bitcoin address for which to get information.
    Text ->
    BitcoindClient AddressInfo

{- | Returns the total available balance.  The available balance is what the
 wallet considers currently spendable, and is thus affected by options which
 limit spendability such as -spendzeroconfchange.

 @since 0.3.0.0
-}
getBalance ::
    -- | Only include transactions confirmed at least this many times. (Default: 0)
    Maybe Int ->
    -- | Also include balance in watch-only addresses (see 'importaddress')
    -- (Default: true for watch-only wallets; otherwise false)
    Maybe Bool ->
    -- | Do not include balance in dirty outputs; addresses are considered dirty
    -- if they have previously been used in a transaction.
    Maybe Bool ->
    -- | The total amount in sats received for this wallet.
    BitcoindClient Word64
getBalance confs watchOnly = fmap toSatoshis . getBalance_ (Just "*") confs watchOnly

getBalance_ ::
    Maybe Text ->
    Maybe Int ->
    Maybe Bool ->
    Maybe Bool ->
    BitcoindClient Scientific

-- | @since 0.3.0.0
data BalanceDetails = BalanceDetails
    { -- | trusted balance (outputs created by the wallet or confirmed outputs)
      balanceDetailsTrusted :: Word64
    , -- | untrusted pending balance (outputs created by others that are in the mempool)
      balanceDetailsUntrustedPending :: Word64
    , -- | balance from immature coinbase outputs
      balanceDetailsImmature :: Word64
    , -- | (only present if avoid_reuse is set) balance from coins sent to
      -- addresses that were previously spent from (potentially privacy violating)
      balanceDetailsUsed :: Maybe Word64
    }
    deriving (Eq, Show)

instance FromJSON BalanceDetails where
    parseJSON = withObject "BalanceDetails" $ \obj ->
        BalanceDetails
            <$> (toSatoshis <$> obj .: "trusted")
            <*> (toSatoshis <$> obj .: "untrusted_pending")
            <*> (toSatoshis <$> obj .: "immature")
            <*> (fmap toSatoshis <$> obj .:? "used")

-- | @since 0.3.0.0
data Balances = Balances
    { -- | balances from outputs that the wallet can sign
      balancesMine :: BalanceDetails
    , -- | watchonly balances (not present if wallet does not watch anything)
      balancesWatchOnly :: Maybe BalanceDetails
    }
    deriving (Eq, Show)

instance FromJSON Balances where
    parseJSON = withObject "Balances" $ \obj ->
        Balances
            <$> obj .: "mine"
            <*> obj .:? "watchonly"

{- | Returns an object with all balances in sats.

 @since 0.3.0.0
-}
getBalances :: BitcoindClient Balances

{- | Returns a new Bitcoin address for receiving payments.
 If 'label' is specified, it is added to the address book
 so payments received with the address will be associated with 'label'.

 @since 0.3.0.0
-}
getNewAddress ::
    -- | The label name for the address to be linked to. It can also be set to
    -- the empty string "" to represent the default label. The label does not need to
    -- exist, it will be created if there is no label by the given name.
    Maybe Text ->
    Maybe AddressType ->
    -- | The new bitcoin address
    BitcoindClient Text

{- | Returns a new Bitcoin address, for receiving change.
 This is for use with raw transactions, NOT normal use.

 @since 0.3.0.0
-}
getRawChangeAddress ::
    Maybe AddressType ->
    -- | The address
    BitcoindClient Text

{- | Returns the total amount received by the given address in transactions with
 at least minconf confirmations.

 @since 0.3.0.0
-}
getReceivedByAddress ::
    -- | The bitcoin address for transactions.
    Text ->
    -- | Only include transactions confirmed at least this many times.
    Maybe Int ->
    -- | The total amount in sats received at this address.
    BitcoindClient Word64
getReceivedByAddress addr = fmap toSatoshis . getReceivedByAddress_ addr

getReceivedByAddress_ :: Text -> Maybe Int -> BitcoindClient Scientific

{- | Returns the total amount received by addresses with <label> in transactions
 with at least [minconf] confirmations.

 @since 0.3.0.0
-}
getReceivedByLabel ::
    -- | The selected label, may be the default label using "".
    Text ->
    -- | Only include transactions confirmed at least this many times.
    Maybe Int ->
    -- | The total amount in sats received for this label.
    BitcoindClient Word64
getReceivedByLabel label = fmap toSatoshis . getReceivedByLabel_ label

getReceivedByLabel_ :: Text -> Maybe Int -> BitcoindClient Scientific

data Category
    = Send
    | Receive
    | Generate
    | Immature
    | Orphan
    deriving (Eq, Show)

instance FromJSON Category where
    parseJSON = withText "Category" $ \case
        "send" -> pure Send
        "receive" -> pure Receive
        "generate" -> pure Generate
        "immature" -> pure Immature
        "orphan" -> pure Orphan
        other -> fail $ "Unknown category: " <> Text.unpack other

data GetTxOutputDetails = GetTxOutputDetails
    { -- | Only returns true if imported addresses were involved in transaction.
      getTxWatchOnly :: Bool
    , -- | The bitcoin address involved in the transaction.
      getTxAddress :: Text
    , getTxCategory :: Category
    , -- | The output amount in sats
      getTxOutputAmount :: Word64
    , -- | A comment for the address/transaction, if any
      getTxLabel :: Maybe Text
    , -- | the vout value
      getTxVout :: Word32
    , getTxOutputFee :: Maybe Word64
    , -- | 'true' if the transaction has been abandoned (inputs are respendable). Only
      -- available for the 'send' category of transactions.
      getTxAbandoned :: Bool
    }
    deriving (Eq, Show)

instance FromJSON GetTxOutputDetails where
    parseJSON = withObject "GetTxOutputDetails" $ \obj ->
        GetTxOutputDetails
            <$> (fromMaybe False <$> obj .:? "involvesWatchOnly")
            <*> obj .: "address"
            <*> obj .: "category"
            <*> (toSatoshis <$> obj .: "amount")
            <*> obj .:? "label"
            <*> obj .: "vout"
            <*> (fmap (toSatoshis . negate) <$> obj .:? "fee")
            <*> (fromMaybe False <$> obj .:? "abandoned")

-- | @since 0.3.0.0
data GetTransactionResponse = GetTransactionResponse
    { -- | The amount in sats
      getTxAmount :: Word64
    , -- | The fee in sats (for "send" transactions)
      getTxFee :: Maybe Word64
    , -- | The number of confirmations for the transaction. Negative
      -- confirmations means the transaction
      -- conflicted that many blocks ago.
      getTxConfs :: Int
    , -- | Only present if transaction only input is a coinbase one.
      getTxGenerated :: Bool
    , -- | Only present if we consider transaction to be trusted and so safe to spend from.
      getTxTrusted :: Bool
    , -- | The block hash containing the transaction.
      getTxBlockId :: BlockHash
    , -- | The block height containing the transaction.
      getTxBlockHeight :: BlockHeight
    , -- | The index of the transaction in the block that includes it.
      getTxBlockIndex :: Int
    , -- | The block time expressed in UNIX epoch time.
      getTxBlockTime :: UTCTime
    , -- | The transaction id.
      getTxId :: TxHash
    , -- | Conflicting transaction ids.
      getTxWalletConflicts :: [Text]
    , -- | The transaction time expressed in UNIX epoch time.
      getTxTime :: UTCTime
    , -- | The time received expressed in UNIX epoch time.
      getTxTimeReceived :: UTCTime
    , -- | If a comment is associated with the transaction, only present if not empty.
      getTxComment :: Maybe Text
    , getTxReplaceable :: Maybe Bool
    , getTxDetails :: [GetTxOutputDetails]
    , getTransactionTx :: Tx
    }
    deriving (Eq, Show)

bip125Bool :: Text -> Maybe Bool
bip125Bool = \case
    "yes" -> pure True
    "no" -> pure False
    _ -> Nothing

instance FromJSON GetTransactionResponse where
    parseJSON = withObject "GetTransactionResponse" $ \obj ->
        GetTransactionResponse
            <$> (toSatoshis <$> obj .: "amount")
            <*> (fmap (toSatoshis . negate) <$> obj .:? "fee")
            <*> obj .: "confirmations"
            <*> (fromMaybe False <$> obj .:? "generated")
            <*> (fromMaybe False <$> obj .:? "trusted")
            <*> obj .: "blockhash"
            <*> obj .: "blockheight"
            <*> obj .: "blockindex"
            <*> (utcTime <$> obj .: "blocktime")
            <*> obj .: "txid"
            <*> obj .: "walletconflicts"
            <*> (utcTime <$> obj .: "time")
            <*> (utcTime <$> obj .: "timereceived")
            <*> obj .:? "comment"
            <*> (bip125Bool <$> obj .: "bip125-replaceable")
            <*> obj .: "details"
            <*> (unHexEncoded <$> obj .: "hex")

{- | Get detailed information about in-wallet transaction <txid>

 @since 0.3.0.0
-}
getTransaction ::
    -- | The transaction id
    TxHash ->
    -- | Whether to include watch-only addresses in balance calculation and details
    Maybe Bool ->
    BitcoindClient GetTransactionResponse

-- | @since 0.3.0.0
data WalletStateInfo = WalletStateInfo
    { -- | the wallet name
      walletStateName :: Text
    , -- | the wallet version
      walletStateVersion :: Int
    , -- | the database format (bdb or sqlite)
      walletStateFormat :: String
    , -- | the total number of transactions in the wallet
      walletStateTxCount :: Int
    , -- | the UNIX epoch time of the oldest pre-generated key in the key pool. Legacy wallets only.
      walletStateKeyPoolOldest :: UTCTime
    , -- | how many new keys are pre-generated (only counts external keys)
      walletStateKeyPoolSize :: Int
    , -- | how many new keys are pre-generated for internal use (used for change
      -- outputs, only appears if the wallet is using this feature, otherwise
      -- external keys are used)
      walletStateKeyPoolSizeHDInternal :: Maybe Int
    , -- | the UNIX epoch time until which the wallet is unlocked for transfers, or 0 if the wallet is locked (only present for passphrase-encrypted wallets)
      walletStateUnlockedUntil :: Maybe UTCTime
    , -- | the transaction fee configuration, set in sats/kvB
      walletStatePayTxFee :: Word64
    , -- | the Hash160 of the HD seed (only present when HD is enabled)
      walletStateHDSeedId :: Maybe Hash160
    , -- | false if privatekeys are disabled for this wallet (enforced watch-only wallet)
      walletStatePrivKeysEnabled :: Bool
    , -- | whether this wallet tracks clean/dirty coins in terms of reuse
      walletStateAvoidReuse :: Bool
    , -- | elapsed seconds since scan start
      walletStateScanningDuration :: Maybe NominalDiffTime
    , -- | scanning progress percentage [0.0, 1.0]
      walletStateScanningProgress :: Maybe Double
    , -- | whether this wallet uses descriptors for scriptPubKey management
      walletStateDescriptors :: Bool
    }
    deriving (Eq, Show)

instance FromJSON WalletStateInfo where
    parseJSON = withObject "WalletStateInfo" $ \obj -> do
        scanning <- obj .: "scanning"
        WalletStateInfo
            <$> obj .: "walletname"
            <*> obj .: "walletversion"
            <*> obj .: "format"
            <*> obj .: "txcount"
            <*> (utcTime <$> obj .: "keypoololdest")
            <*> obj .: "keypoolsize"
            <*> obj .:? "keypoolsize_hd_internal"
            <*> (fmap utcTime <$> obj .:? "unlocked_until")
            <*> (toSatoshis <$> obj .: "paytxfee")
            <*> (fmap unHexEncoded <$> obj .:? "hdseedid")
            <*> obj .: "private_keys_enabled"
            <*> obj .: "avoid_reuse"
            <*> getField "duration" scanning
            <*> getField "progress" scanning
            <*> obj .: "descriptors"
      where
        getField field (Object obj) = Just <$> obj .: field
        getField _ _ = pure Nothing

{- | Returns an object containing various wallet state info.

 @since 0.3.0.0
-}
getWalletInfo :: BitcoindClient WalletStateInfo

{- | Adds an address or script (in hex) that can be watched as if it were in your wallet but cannot be used to spend. Requires a new wallet backup.

 Note: This call can take over an hour to complete if rescan is true, during that time, other rpc calls
 may report that the imported address exists but related transactions are still missing, leading to temporarily incorrect/bogus balances and unspent outputs until rescan completes.
 If you have the full public key, you should call importpubkey instead of this.
 Hint: use importmulti to import more than one address.

 Note: If you import a non-standard raw script in hex form, outputs sending to it will be treated
 as change, and not show up in many RPCs.
 Note: Use "getwalletinfo" to query the scanning progress.

 @since 0.3.0.0
-}
importAddress ::
    -- | The Bitcoin address (or hex-encoded script)
    Text ->
    -- | An optional label
    Maybe Text ->
    -- | Rescan the wallet for transactions
    Maybe Bool ->
    -- | Add the P2SH version of the script as well
    Maybe Bool ->
    BitcoindClient ()

-- | @since 0.3.0.0
data DescriptorRequest = DescriptorRequest
    { -- | Descriptor to import.
      descriptorRequestDesc :: Text
    , -- | Set this descriptor to be the active descriptor for the corresponding output type/externality (Default: false)
      descriptorRequestActive :: Maybe Bool
    , -- | If a ranged descriptor is used, this specifies the end or the range (in the form [begin,end]) to import
      descriptorRequestRange :: Maybe (Int, Maybe Int)
    , -- | If a ranged descriptor is set to active, this specifies the next index to generate addresses from
      descriptorRequestNextIndex :: Maybe Int
    , -- | Time from which to start rescanning the blockchain for this descriptor, in UNIX epoch time
      -- Use the string "now" to substitute the current synced blockchain time.
      -- "now" can be specified to bypass scanning, for outputs which are known to never have been used, and
      -- 0 can be specified to scan the entire blockchain. Blocks up to 2 hours before the earliest timestamp
      -- of all descriptors being imported will be scanned.
      descriptorRequestTimestamp :: Maybe UTCTime
    , -- | Whether matching outputs should be treated as not incoming payments (e.g. change) (Default: false)
      descriptorRequestInternal :: Maybe Bool
    , -- | Label to assign to the address, only allowed with internal=false (Default: "")
      descriptorRequestLabel :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON DescriptorRequest where
    toJSON req =
        partialObject
            [ Just $ "desc" .= descriptorRequestDesc req
            , "active" .=? descriptorRequestActive req
            , "range" .=? (fmap rangeToJSON . descriptorRequestRange) req
            , "next_index" .=? descriptorRequestNextIndex req
            , Just $ "timestamp" .= (maybe (toJSON @Text "now") toJSON . descriptorRequestTimestamp) req
            , "internal" .=? descriptorRequestInternal req
            , "label" .=? descriptorRequestLabel req
            ]

-- | @since 0.3.0.0
data ImportResponse = ImportResponse
    { importResponseSuccess :: Bool
    , importResponseWarnings :: [Text]
    , importResponseError :: Maybe Value
    }
    deriving (Eq, Show)

instance FromJSON ImportResponse where
    parseJSON = withObject "ImportResponse" $ \obj ->
        ImportResponse
            <$> obj .: "success"
            <*> (fromMaybe mempty <$> obj .:? "warnings")
            <*> obj .:? "error"

{- | Import descriptors. This will trigger a rescan of the blockchain based on
 the earliest timestamp of all descriptors being imported. Requires a new wallet
 backup.

  Note: This call can take over an hour to complete if using an early timestamp;
  during that time, other rpc calls may report that the imported keys, addresses
  or scripts exist but related transactions are still missing.

  @since 0.3.0.0
-}
importDescriptors ::
    [DescriptorRequest] ->
    BitcoindClient [ImportResponse]

-- | @since 0.3.0.0
data ImportScriptPubKey = ImportScript Text | ImportAddress Text
    deriving (Eq, Show)

instance ToJSON ImportScriptPubKey where
    toJSON = \case
        ImportScript script -> toJSON script
        ImportAddress addr -> object ["address" .= addr]

-- | @since 0.3.0.0
data ImportMultiRequest = ImportMultiRequest
    { -- | Descriptor to import. If using descriptor, do not also provide address/scriptPubKey, scripts, or pubkeys
      importMultiDesc :: Maybe Text
    , -- | Type of scriptPubKey (string for script, json for address). Should not be provided if using a descriptor
      importMultiScriptPubKey :: Maybe ImportScriptPubKey
    , -- | Creation time of the key expressed in UNIX epoch time,
      -- or the string "now" to substitute the current synced blockchain time. The timestamp of the oldest
      -- key will determine how far back blockchain rescans need to begin for missing wallet transactions.
      -- "now" can be specified to bypass scanning, for keys which are known to never have been used, and
      -- 0 can be specified to scan the entire blockchain. Blocks up to 2 hours before the earliest key
      -- creation time of all keys being imported by the importmulti call will be scanned.
      importMultiTimestamp :: Maybe UTCTime
    , -- | Allowed only if the scriptPubKey is a P2SH or P2SH-P2WSH address/scriptPubKey
      importMultiRedeemScript :: Maybe Text
    , -- | Allowed only if the scriptPubKey is a P2SH-P2WSH or P2WSH address/scriptPubKey
      importMultiWitnessScript :: Maybe Text
    , -- | Array of strings giving pubkeys to import. They must occur in P2PKH
      -- or P2WPKH scripts. They are not required when the private key is also
      -- provided (see the "keys" argument).
      importMultiPubkeys :: [Text]
    , -- | Array of strings giving private keys to import. The corresponding
      -- public keys must occur in the output or redeemscript.
      importMultiKeys :: [Text]
    , -- | If a ranged descriptor is used, this specifies the end or the range
      -- (in the form [begin,end]) to import
      importMultiRange :: Maybe (Int, Maybe Int)
    , -- | Stating whether matching outputs should be treated as not incoming
      -- payments (also known as change)
      importMultiInternal :: Maybe Bool
    , -- | Stating whether matching outputs should be considered watchonly.
      importMultiWatchOnly :: Maybe Bool
    , -- | Label to assign to the address, only allowed with internal=false
      importMultiLabel :: Maybe Text
    , -- | Stating whether imported public keys should be added to the keypool
      -- for when users request new addresses. Only allowed when wallet private keys
      -- are disabled
      importMultiKeypool :: Maybe Bool
    }
    deriving (Eq, Show)

instance ToJSON ImportMultiRequest where
    toJSON req =
        partialObject
            [ "desc" .=? importMultiDesc req
            , "scriptPubKey" .=? importMultiScriptPubKey req
            , Just $ "timestamp" .= (maybe (toJSON @Text "now") toJSON . importMultiTimestamp) req
            , "reedemscript" .=? importMultiRedeemScript req
            , "witnessscript" .=? importMultiWitnessScript req
            , Just $ "pubkeys" .= importMultiPubkeys req
            , Just $ "keys" .= importMultiKeys req
            , "range" .=? (fmap rangeToJSON . importMultiRange) req
            , "internal" .=? importMultiInternal req
            , "watchonly" .=? importMultiWatchOnly req
            , "label" .=? importMultiLabel req
            , "keypool" .=? importMultiKeypool req
            ]

newtype ImportMultiOptions = ImportMultiOptions Bool
    deriving (Eq, Show)

instance ToJSON ImportMultiOptions where
    toJSON (ImportMultiOptions doRescan) = object ["rescan" .= doRescan]

importMulti_ ::
    [ImportMultiRequest] ->
    Maybe ImportMultiOptions ->
    BitcoindClient [ImportResponse]

{- | Import addresses/scripts (with private or public keys, redeem script
 (P2SH)), optionally rescanning the blockchain from the earliest creation time
 of the imported scripts. Requires a new wallet backup.  If an address/script is
 imported without all of the private keys required to spend from that address,
 it will be watchonly. The 'watchonly' option must be set to true in this case
 or a warning will be returned.  Conversely, if all the private keys are
 provided and the address/script is spendable, the watchonly option must be set
 to false, or a warning will be returned.

 Note: This call can take over an hour to complete if rescan is true, during
 that time, other rpc calls may report that the imported keys, addresses or
 scripts exist but related transactions are still missing. Note: Use
 "getwalletinfo" to query the scanning progress.

 @since 0.3.0.0
-}
importMulti ::
    [ImportMultiRequest] ->
    -- | Stating if should rescan the blockchain after all imports
    Maybe Bool ->
    BitcoindClient [ImportResponse]
importMulti req = importMulti_ req . fmap ImportMultiOptions

{- | Adds a private key (as returned by dumpprivkey) to your wallet. Requires a
 new wallet backup.  Hint: use importmulti to import more than one private key.

 Note: This call can take over an hour to complete if rescan is true, during
 that time, other rpc calls may report that the imported key exists but related
 transactions are still missing, leading to temporarily incorrect/bogus balances
 and unspent outputs until rescan completes. Note: Use "getwalletinfo" to query
 the scanning progress.

 @since 0.3.0.0
-}
importPrivKey ::
    -- | The private key (see dumpprivkey)
    Text ->
    -- | An optional label
    Maybe Text ->
    -- | Rescan the wallet for transactions
    Maybe Bool ->
    BitcoindClient ()

{- | Imports keys from a wallet dump file (see dumpwallet). Requires a new
 wallet backup to include imported keys. Note: Use "getwalletinfo" to query the
 scanning progress.

 @since 0.3.0.0
-}
importWallet ::
    -- | The wallet file
    FilePath ->
    BitcoindClient ()

-- | @since 0.3.0.0
data DescriptorDetails = DescriptorDetails
    { -- | Descriptor string representation
      descriptorDesc :: Text
    , -- | The creation time of the descriptor
      descriptorTimestamp :: UTCTime
    , -- | Activeness flag
      descriptorActive :: Bool
    , -- | Whether this is internal or external descriptor; defined only for
      -- active descriptors
      descriptorInternal :: Bool
    , -- | Defined only for ranged descriptors
      descriptorRange :: Maybe (Int, Int)
    , -- | The next index to generate addresses from; defined only for ranged
      -- descriptors
      descriptorNext :: Maybe Int
    }
    deriving (Eq, Show)

instance FromJSON DescriptorDetails where
    parseJSON = withObject "DescriptorDetails" $ \obj ->
        DescriptorDetails
            <$> obj .: "desc"
            <*> (utcTime <$> obj .: "timestamp")
            <*> obj .: "active"
            <*> obj .: "internal"
            <*> (obj .:? "range" >>= rangeFromJSON)
            <*> obj .: "next"
      where
        rangeFromJSON = \case
            Just [start, end] -> pure $ Just (start, end)
            Just{} -> fail "Malformed range"
            Nothing -> pure Nothing

{- | List descriptors imported into a descriptor-enabled wallet.  Supported
starting in version @0.21.1@.

 @since 0.3.0.0
-}
listDescriptors :: BitcoindClient [DescriptorDetails]

{- | Returns the list of all labels, or labels that are assigned to addresses
 with a specific purpose.

 @since 0.3.0.0
-}
listLabels ::
    -- | Address purpose to list labels for ('send','receive'). An empty string
    -- is the same as not providing this argument.
    Maybe Purpose ->
    BitcoindClient [Text]
listLabels = listLabels_ . fmap purposeText

listLabels_ :: Maybe Text -> BitcoindClient [Text]

newtype JsonOutPoint = JsonOutPoint {unJsonOutPoint :: OutPoint}
    deriving (Eq, Show)

instance FromJSON JsonOutPoint where
    parseJSON = withObject "JsonOutPoint" $ \obj ->
        fmap JsonOutPoint $ OutPoint <$> obj .: "txid" <*> obj .: "vout"

listLockUnspent_ :: BitcoindClient [JsonOutPoint]

{- | Returns list of temporarily unspendable outputs. See the 'lockUnspent' call
 to lock and unlock transactions for spending.

 @since 0.3.0.0
-}
listLockUnspent :: BitcoindClient [OutPoint]
listLockUnspent = fmap unJsonOutPoint <$> listLockUnspent_

-- | @since 0.3.0.0
data ListReceivedResponse = ListReceivedResponse
    { -- | Only returns true if imported addresses were involved in transaction
      listReceivedWatchOnly :: Bool
    , -- | The receiving address
      listReceivedAddress :: Text
    , -- | The total amount in sats received by the address
      listReceivedAmount :: Word64
    , -- | The number of confirmations of the most recent transaction included
      listReceivedConfs :: Int
    , -- | The label of the receiving address. The default label is ""
      listReceivedLabel :: Text
    , -- | The ids of transactions received with the address
      listReceivedTxIds :: [TxHash]
    }
    deriving (Eq, Show)

instance FromJSON ListReceivedResponse where
    parseJSON = withObject "ListReceivedResponse" $ \obj ->
        ListReceivedResponse
            <$> (fromMaybe False <$> obj .:? "involvesWatchOnly")
            <*> obj .: "address"
            <*> (toSatoshis <$> obj .: "amount")
            <*> obj .: "confirmations"
            <*> obj .: "label"
            <*> obj .: "txids"

{- | List balances by receiving address.

 @since 0.3.0.0
-}
listReceivedByAddress ::
    -- | The minimum number of confirmations before payments are included.
    Maybe Int ->
    -- | Whether to include addresses that haven't received any payments.
    Maybe Bool ->
    -- | Whether to include watch-only addresses (see 'importaddress')
    Maybe Bool ->
    -- | If present, only return information on this address.
    Maybe Text ->
    BitcoindClient [ListReceivedResponse]

-- | @since 0.3.0.0
data ListReceivedByLabelResponse = ListReceivedByLabelResponse
    { -- | Only returns true if imported addresses were involved in transaction
      listRecvByLabelWatchOnly :: Bool
    , -- | The total amount received by addresses with this label
      listRecvByLabelAmount :: Word64
    , -- | The number of confirmations of the most recent transaction included
      listRecvByLabelConfs :: Int
    , -- | The label of the receiving address. The default label is ""
      listRecvByLabelLabel :: Text
    }
    deriving (Eq, Show)

instance FromJSON ListReceivedByLabelResponse where
    parseJSON = withObject "ListReceivedByLabelResponse" $ \obj ->
        ListReceivedByLabelResponse
            <$> (fromMaybe False <$> obj .:? "involvesWatchOnly")
            <*> obj .: "amount"
            <*> obj .: "confirmations"
            <*> obj .: "label"

-- | List received transactions by label.
listReceivedByLabel ::
    -- | The minimum number of confirmations before payments are included.
    Maybe Int ->
    -- | Whether to include labels that haven't received any payments.
    Maybe Bool ->
    -- | Whether to include watch-only addresses (see 'importAddress')
    Maybe Bool ->
    BitcoindClient [ListReceivedByLabelResponse]

-- | @since 0.3.0.0
data TransactionDetails = TransactionDetails
    { -- | Only returns true if imported addresses were involved in transaction.
      txDetailsWatchOnly :: Bool
    , -- | The bitcoin address involved in the transaction.
      txDetailsAddress :: Text
    , -- | The transaction category.
      txDetailsCategory :: Category
    , -- | The amount in sats
      txDetailsAmount :: Word64
    , -- | A comment for the address/transaction, if any
      txDetailsLabel :: Maybe Text
    , -- | The vout value
      txDetailsVout :: Word32
    , -- | The amount of the fee in sats. This is only available for
      -- the 'send' category of transactions.
      txDetailsFee :: Maybe Word64
    , -- | 'true' if the transaction has been abandoned (inputs are
      -- respendable). Only available for the 'send' category of transactions.
      txDetailsAbandoned :: Maybe Bool
    , -- | The number of confirmations for the transaction. Negative confirmations means the transaction conflicted that many blocks ago.
      txDetailsConfs :: Int
    , -- | Only present if transaction only input is a coinbase one.
      txDetailsGenerated :: Bool
    , -- | Only present if we consider transaction to be trusted and so safe to spend from.
      txDetailsTrusted :: Bool
    , -- | The block hash containing the transaction.
      txDetailsBlockHash :: BlockHash
    , -- | The block height containing the transaction.
      txDetailsBlockHeight :: BlockHeight
    , -- | The index of the transaction in the block that includes it.
      txDetailsBlockIndex :: Int
    , -- | The block time expressed in UNIX epoch time.
      txDetailsBlockTime :: UTCTime
    , -- | The transaction id.
      txDetailsTxId :: TxHash
    , -- | Conflicting transaction ids.
      txDetailsConflicts :: [TxHash]
    , -- | The transaction time
      txDetailsTime :: UTCTime
    , -- | The time received
      txDetailsReceivedTime :: UTCTime
    , -- | Whether this transaction could be replaced due to BIP125
      -- (replace-by-fee); may be unknown for unconfirmed transactions not in the
      -- mempool
      txDetailsReplaceable :: Maybe Bool
    , -- | If a comment to is associated with the transaction.
      txDetailsTo :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON TransactionDetails where
    parseJSON = withObject "TransactionDetails" $ \obj ->
        TransactionDetails
            <$> (fromMaybe False <$> obj .:? "involvesWatchOnly")
            <*> obj .: "address"
            <*> obj .: "category"
            <*> (toSatoshis <$> obj .: "amount")
            <*> obj .:? "label"
            <*> obj .: "vout"
            <*> (fmap (toSatoshis . negate) <$> obj .:? "fee")
            <*> obj .:? "abandoned"
            <*> obj .: "confirmations"
            <*> (fromMaybe False <$> obj .:? "generated")
            <*> (fromMaybe False <$> obj .:? "trusted")
            <*> obj .: "blockhash"
            <*> obj .: "blockheight"
            <*> obj .: "blockindex"
            <*> (utcTime <$> obj .: "blocktime")
            <*> obj .: "txid"
            <*> obj .: "walletconflicts"
            <*> (utcTime <$> obj .: "time")
            <*> (utcTime <$> obj .: "timereceived")
            <*> ((>>= bip125Bool) <$> obj .:? "bip125-replaceable")
            <*> obj .:? "to"

-- | @since 0.3.0.0
data ListSinceBlockResponse = ListSinceBlockResponse
    { listSinceBlockTxs :: [TransactionDetails]
    , listSinceBlockRemoved :: [TransactionDetails]
    , listSinceBlockLastBlock :: BlockHash
    }
    deriving (Eq, Show)

instance FromJSON ListSinceBlockResponse where
    parseJSON = withObject "ListSinceBlockResponse" $ \obj ->
        ListSinceBlockResponse
            <$> obj .: "transactions"
            <*> (fromMaybe mempty <$> obj .: "removed")
            <*> obj .: "lastblock"

{- | Get all transactions in blocks since block [blockhash], or all transactions
 if omitted.  If "blockhash" is no longer a part of the main chain,
 transactions from the fork point onward are included.  Additionally, if
 include_removed is set, transactions affecting the wallet which were removed
 are returned in the "removed" array.

 @since 0.3.0.0
-}
listSinceBlock ::
    -- | If set, the block hash to list transactions since, otherwise list all
    -- transactions.
    Maybe BlockHash ->
    -- | Return the nth block hash from the main chain. e.g. 1 would mean the
    -- best block hash. Note: this is not used as a filter, but only affects
    -- [lastblock] in the return value
    Maybe Int ->
    -- | Include transactions to watch-only addresses (see 'importaddress')
    Maybe Bool ->
    -- | Show transactions that were removed due to a reorg in the "removed" array
    -- (not guaranteed to work on pruned nodes)
    Maybe Bool ->
    BitcoindClient ListSinceBlockResponse

{- | If a label name is provided, this will return only incoming transactions
 paying to addresses with the specified label. Returns up to 'count' most
 recent transactions skipping the first 'from' transactions.

 @since 0.3.0.0
-}
listTransactions ::
    -- | If set, should be a valid label name to return only incoming
    -- transactions with the specified label, or "*" to disable filtering and
    -- return all transactions.
    Maybe Text ->
    -- | The number of transactions to return (default: 10)
    Maybe Int ->
    -- | The number of transactions to skip
    Maybe Int ->
    -- | Include transactions to watch-only addresses (see 'importAddress')
    Maybe Bool ->
    BitcoindClient [TransactionDetails]

-- | @since 0.3.0.0
data ListUnspentOptions = ListUnspentOptions
    { -- | Minimum value of each UTXO in sats
      listUnspentMinAmount :: Maybe Word64
    , -- | Maximum value of each UTXO in sats
      listUnspentMaxAmount :: Maybe Word64
    , -- | Maximum number of UTXOs
      listUnspentMaxCount :: Maybe Int
    , -- | Minimum sum value of all UTXOs in sats
      listUnspentMinSumAmount :: Maybe Word64
    }
    deriving (Eq, Show)

instance ToJSON ListUnspentOptions where
    toJSON opts =
        partialObject
            [ "minimumAmount" .=? (satsToBTCText <$> listUnspentMinAmount opts)
            , "maximumAmount" .=? (satsToBTCText <$> listUnspentMaxAmount opts)
            , "maximumCount" .=? listUnspentMaxCount opts
            , "minimumSumAmount" .=? (satsToBTCText <$> listUnspentMinSumAmount opts)
            ]

-- | @since 0.3.0.0
data OutputDetails = OutputDetails
    { -- | the transaction id
      outputTxId :: TxHash
    , -- | the vout value
      outputVOut :: Word32
    , -- | the bitcoin address
      outputAddress :: Text
    , -- | The associated label, or "" for the default label
      outputLabel :: Maybe Text
    , -- | the script key
      outputScriptPubKey :: Text
    , -- | the transaction output amount in sats
      outputAmount :: Word64
    , -- | The number of confirmations
      outputConfs :: Int
    , -- | The redeemScript if scriptPubKey is P2SH
      outputRedeemScript :: Maybe Text
    , -- | witnessScript if the scriptPubKey is P2WSH or P2SH-P2WSH
      outputWitnessScript :: Maybe Text
    , -- | Whether we have the private keys to spend this output
      outputSpendable :: Bool
    , -- | Whether we know how to spend this output, ignoring the lack of keys
      outputSolvable :: Bool
    , -- | Whether this output is reused/dirty (sent to an address that was previously spent from)
      outputReused :: Maybe Bool
    , -- | A descriptor for spending this output
      outputDescriptor :: Maybe Text
    , -- | Whether this output is considered safe to spend. Unconfirmed transactions
      -- from outside keys and unconfirmed replacement transactions are considered unsafe
      -- and are not eligible for spending by fundrawtransaction and sendtoaddress.
      outputSafe :: Bool
    }
    deriving (Eq, Show)

instance FromJSON OutputDetails where
    parseJSON = withObject "OutputDetails" $ \obj ->
        OutputDetails
            <$> obj .: "txid"
            <*> obj .: "vout"
            <*> obj .: "address"
            <*> obj .:? "label"
            <*> obj .: "scriptPubKey"
            <*> (toSatoshis <$> obj .: "amount")
            <*> obj .: "confirmations"
            <*> obj .:? "redeemScript"
            <*> obj .:? "witnessScript"
            <*> obj .: "spendable"
            <*> obj .: "solvable"
            <*> obj .:? "reused"
            <*> obj .:? "desc"
            <*> obj .: "safe"

{- | Returns array of unspent transaction outputs with between minconf and maxconf
 (inclusive) confirmations.  Optionally filter to only include txouts paid to
 specified addresses.

 @since 0.3.0.0
-}
listUnspent ::
    -- | The minimum confirmations to filter
    Maybe Int ->
    -- | The maximum confirmations to filter
    Maybe Int ->
    -- | The bitcoin addresses to filter
    Maybe [Text] ->
    -- | Include outputs that are not safe to spend
    Maybe Bool ->
    ListUnspentOptions ->
    BitcoindClient [OutputDetails]

{- | Returns a list of currently loaded wallets.
 For full information on the wallet, use 'getWalletInfo'

 @since 0.3.0.0
-}
listWallets :: BitcoindClient [Text]

{- | Loads a wallet from a wallet file or directory.  Note that all wallet
 command-line options used when starting bitcoind will be applied to the new
 wallet (eg -rescan, etc).

 @since 0.3.0.0
-}
loadWallet ::
    -- | The wallet directory or .dat file.
    Text ->
    -- | Save wallet name to persistent settings and load on startup. True to
    -- add wallet to startup list, false to remove, null to leave unchanged.
    Maybe Bool ->
    BitcoindClient LoadWalletResponse

{- | Updates list of temporarily unspendable outputs.  Temporarily lock
 (unlock=false) or unlock (unlock=true) specified transaction outputs.  If no
 transaction outputs are specified when unlocking then all current locked
 transaction outputs are unlocked.  A locked transaction output will not be
 chosen by automatic coin selection, when spending bitcoins.  Manually selected
 coins are automatically unlocked.  Locks are stored in memory only. Nodes start
 with zero locked outputs, and the locked output list is always cleared (by
 virtue of process exit) when a node stops or fails.  Also see the listunspent
 call

 @since 0.3.0.0
-}
lockUnspent ::
    -- | Whether to unlock (true) or lock (false) the specified transactions
    Bool ->
    -- | The transaction outputs and within each, the txid (string) vout (numeric).
    [PrevTx] ->
    BitcoindClient Bool

{- | Bumps the fee of an opt-in-RBF transaction T, replacing it with a new
 transaction B.  Returns a PSBT instead of creating and signing a new
 transaction.  An opt-in RBF transaction with the given txid must be in the
 wallet.  The command will pay the additional fee by reducing change outputs or
 adding inputs when necessary.  It may add a new change output if one does not
 already exist.  All inputs in the original transaction will be included in the
 replacement transaction.  The command will fail if the wallet or mempool
 contains a transaction that spends one of T's outputs.  By default, the new fee
 will be calculated automatically using the estimatesmartfee RPC.  The user can
 specify a confirmation target for estimatesmartfee.  Alternatively, the user
 can specify a fee rate in sat/vB for the new transaction.  At a minimum, the
 new fee rate must be high enough to pay an additional new relay fee
 (incrementalfee returned by getnetworkinfo) to enter the node's mempool.  *
 WARNING: before version 0.21, fee_rate was in BTC/kvB. As of 0.21, fee_rate is
 in sat/vB. *

 @since 0.3.0.0
-}
psbtBumpFee ::
    -- | The txid to be bumped
    TxHash ->
    Maybe BumpFeeOptions ->
    BitcoindClient BumpFeeResponse

-- | @since 0.3.0.0
data RescanResponse = RescanResponse
    { -- | The block height where the rescan started (the requested height or 0)
      rescanStart :: Int
    , -- | The height of the last rescanned block. May be null in rare cases if
      -- there was a reorg and the call didn't scan any blocks because they were
      -- already scanned in the background.
      rescanStop :: Maybe Int
    }
    deriving (Eq, Show)

instance FromJSON RescanResponse where
    parseJSON = withObject "RescanResponse" $ \obj ->
        RescanResponse
            <$> obj .: "start_height"
            <*> obj .:? "stop_height"

{- | Rescan the local blockchain for wallet related transactions.
 Note: Use 'getWalletInfo' to query the scanning progress.

 @since 0.3.0.0
-}
rescanBlockchain ::
    Maybe BlockHeight ->
    Maybe BlockHeight ->
    BitcoindClient RescanResponse

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

{- | Send multiple times. Amounts are double-precision floating point numbers.
 Requires wallet passphrase to be set with walletpassphrase call if wallet is
 encrypted.

 @since 0.3.0.0
-}
sendMany ::
    -- | The bitcoin address is the key, the amount in sats is the value
    Map Text Word64 ->
    -- | A comment
    Maybe Text ->
    -- | The addresses.
    -- The fee will be equally deducted from the amount of each selected address.
    -- Those recipients will receive less bitcoins than you enter in their corresponding amount field.
    -- If no addresses are specified here, the sender pays the fee.
    [Text] ->
    -- | Allow this transaction to be replaced by a transaction with higher fees via BIP 125
    Maybe Bool ->
    -- | Confirmation target in blocks
    Maybe Int ->
    Maybe FeeEstimationMode ->
    -- | Specify a fee rate in sat/vB.
    Maybe Word64 ->
    BitcoindClient TxHash
sendMany = sendMany_ . fmap satsToBTCText

sendMany_ ::
    Map Text Text ->
    Maybe Text ->
    [Text] ->
    Maybe Bool ->
    Maybe Int ->
    Maybe FeeEstimationMode ->
    Maybe Word64 ->
    BitcoindClient TxHash

{- | Send an amount to a given address.  Requires wallet passphrase to be set
 with walletpassphrase call if wallet is encrypted.

 @since 0.3.0.0
-}
sendToAddress ::
    -- | The bitcoin address to send to.
    Text ->
    -- | The amount in sats to send
    Word64 ->
    -- | A comment used to store what the transaction is for. This is not part
    -- of the transaction, just kept in your wallet.
    Maybe Text ->
    -- | A comment to store the name of the person or organization
    -- to which you're sending the transaction. This is not part of the
    -- transaction, just kept in your wallet.
    Maybe Text ->
    -- | The fee will be deducted from the amount being sent.
    -- The recipient will receive less bitcoins than you enter in the amount field.
    Maybe Bool ->
    -- | Allow this transaction to be replaced by a transaction with higher fees via BIP 125
    Maybe Bool ->
    -- | Confirmation target in blocks
    Maybe Int ->
    Maybe FeeEstimationMode ->
    -- | Avoid spending from dirty addresses; addresses are considered dirty if
    -- they have previously been used in a transaction.
    Maybe Bool ->
    -- | Specify a fee rate in sat/vB.
    Maybe Word64 ->
    BitcoindClient TxHash
sendToAddress addr = sendToAddress_ addr . satsToBTCText

sendToAddress_ ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Bool ->
    Maybe Bool ->
    Maybe Int ->
    Maybe FeeEstimationMode ->
    Maybe Bool ->
    Maybe Word64 ->
    BitcoindClient TxHash

-- | Sets the label associated with the given address.
setLabel ::
    -- | The bitcoin address to be associated with a label.
    Text ->
    -- | The label to assign to the address.
    Text ->
    BitcoindClient ()

{- | Set the transaction fee per kB for this wallet. Overrides the global
 -paytxfee command line parameter.  Can be deactivated by passing 0 as the fee.
 In that case automatic fee selection will be used by default.

 @since 0.3.0.0
-}
setTxFee ::
    -- | The transaction fee in sats/kvB
    Word64 ->
    -- | Returns true if successful
    BitcoindClient Bool
setTxFee = setTxFee_ . satsToBTCText

setTxFee_ :: Text -> BitcoindClient Bool

{- | Sign a message with the private key of an address
 Requires wallet passphrase to be set with walletpassphrase call if wallet is encrypted.

 @since 0.3.0.0
-}
signMessage ::
    -- | The bitcoin address to use for the private key.
    Text ->
    -- | The message to create a signature of.
    Text ->
    -- | The signature of the message encoded in base 64
    BitcoindClient Text

data PrevTx = PrevTx
    { -- | The transaction id
      prevTxId :: TxHash
    , -- | The output number
      prevTxVOut :: Word32
    , -- | script key
      prevTxScriptPubKey :: Text
    , -- | redeem script (required for P2SH)
      prevTxRedeemScript :: Maybe Text
    , -- | witness script (required for P2WSH or P2SH-P2WSH)
      prevTxWitnessScript :: Maybe Text
    , -- | the amount spent (required for segwit inputs)
      prevTxAmount :: Word64
    }
    deriving (Eq, Show)

instance ToJSON PrevTx where
    toJSON tx =
        partialObject
            [ Just $ "txid" .= prevTxId tx
            , Just $ "vout" .= prevTxVOut tx
            , Just $ "scriptPubKey" .= prevTxScriptPubKey tx
            , "reedemScript" .=? prevTxRedeemScript tx
            , "witnessScript" .=? prevTxWitnessScript tx
            , Just $ "amount" .= prevTxAmount tx
            ]

data SignRawTxError = SignRawTxError
    { signRawTxErrorTxId :: TxHash
    , signRawTxErrorVOut :: Word32
    , signRawTxErrorScriptSig :: Text
    , signRawTxErrorSequence :: Int
    , signRawTxError :: Text
    }
    deriving (Eq, Show)

instance FromJSON SignRawTxError where
    parseJSON = withObject "SignRawTxError" $ \obj ->
        SignRawTxError
            <$> obj .: "txid"
            <*> obj .: "vout"
            <*> obj .: "scriptSig"
            <*> obj .: "sequence"
            <*> obj .: "error"

data SignRawTxResponse = SignRawTxResponse
    { signRawTxHex :: Text
    , signRawTxComplete :: Bool
    , signRawTxErrors :: [SignRawTxError]
    }
    deriving (Eq, Show)

instance FromJSON SignRawTxResponse where
    parseJSON = withObject "SignRawTxResponse" $ \obj ->
        SignRawTxResponse
            <$> obj .: "hex"
            <*> obj .: "complete"
            <*> obj .: "errors"

{- | Sign inputs for raw transaction (serialized, hex-encoded).  The second
 optional argument (may be null) is an array of previous transaction outputs
 that this transaction depends on but may not yet be in the block chain.
 Requires wallet passphrase to be set with walletpassphrase call if wallet is
 encrypted.

 @since 0.3.0.0
-}
signRawTx ::
    -- | The transaction hex string
    Text ->
    -- | The previous dependent transaction outputs
    [PrevTx] ->
    -- | The signature hash type. Must be one of "ALL" "NONE" "SINGLE"
    -- "ALL|ANYONECANPAY" "NONE|ANYONECANPAY" "SINGLE|ANYONECANPAY"
    Maybe Text ->
    BitcoindClient SignRawTxResponse

newtype UnloadWalletResponse = UnloadWalletResponse {unUnloadWalletResponse :: Text}
    deriving (Eq, Show)

instance FromJSON UnloadWalletResponse where
    parseJSON = withObject "UnloadWalletResponse" $ fmap UnloadWalletResponse . (.: "warning")

unloadWallet_ ::
    Maybe Text ->
    Maybe Bool ->
    BitcoindClient UnloadWalletResponse

{- | Unloads the wallet referenced by the request endpoint otherwise unloads the
wallet specified in the argument.  Specifying the wallet name on a wallet
endpoint is invalid.

 @since 0.3.0.0
-}
unloadWallet ::
    -- | The name of the wallet to unload. If provided both here and in the RPC
    -- endpoint, the two must be identical.
    Maybe Text ->
    -- | Save wallet name to persistent settings and load on startup. True to
    -- add wallet to startup list, false to remove, null to leave unchanged.
    Maybe Bool ->
    BitcoindClient Text
unloadWallet name = fmap unUnloadWalletResponse . unloadWallet_ name

-- | @since 0.3.0.0
data CreatePsbtOptions = CreatePsbtOptions
    { -- | If inputs are specified, automatically include more if they are not enough.
      createPsbtAddInputs :: Maybe Bool
    , -- | The bitcoin address to receive the change
      createPsbtChangeAddress :: Maybe Text
    , -- | The index of the change output
      createPsbtChangePosition :: Maybe Int
    , -- | The output type to use. Only valid if changeAddress is not specified.
      createPsbtChangeType :: Maybe AddressType
    , -- | Also select inputs which are watch only
      createPsbtIncludeWatching :: Maybe Bool
    , -- | Lock selected unspent outputs
      createPsbtLockUnspents :: Maybe Bool
    , -- | Specify a fee rate in sat/vB.
      createPsbtFeeRate :: Maybe Word64
    , -- | The outputs to subtract the fee from. The fee will be equally
      -- deducted from the amount of each specified output. Those recipients will
      -- receive less bitcoins than you enter in their corresponding amount field. If
      -- no outputs are specified here, the sender pays the fee.
      --
      -- (zero-based output index, before a change output is added)
      createPsbtSubtractFee :: [Int]
    , -- | Marks this transaction as BIP125 replaceable. Allows this transaction
      -- to be replaced by a transaction with higher fees
      createPsbtReplaceable :: Maybe Bool
    , -- | Confirmation target in blocks
      createPsbtConfTarget :: Maybe Int
    , createPsbtEstimateMode :: Maybe FeeEstimationMode
    }
    deriving (Eq, Show)

instance ToJSON CreatePsbtOptions where
    toJSON opts =
        partialObject
            [ "add_inputs" .=? createPsbtAddInputs opts
            , "changeAddress" .=? createPsbtChangeAddress opts
            , "changePosition" .=? createPsbtChangePosition opts
            , "change_type" .=? createPsbtChangeType opts
            , "includeWatching" .=? createPsbtIncludeWatching opts
            , "lockUnspents" .=? createPsbtLockUnspents opts
            , "fee_rate" .=? createPsbtFeeRate opts
            , Just $ "subtractFeeFromOutputs" .= createPsbtSubtractFee opts
            , "replaceable" .=? createPsbtReplaceable opts
            , "conf_target" .=? createPsbtConfTarget opts
            , "estimate_mode" .=? createPsbtEstimateMode opts
            ]

-- | @since 0.3.0.0
data CreatePsbtResponse = CreatePsbtResponse
    { createPsbtPsbt :: PartiallySignedTransaction
    , -- | Fee in sats the resulting transaction pays
      createPsbtFee :: Word64
    , -- | The position of the added change output, or -1
      createPsbtChangePos :: Int
    }
    deriving (Eq, Show)

instance FromJSON CreatePsbtResponse where
    parseJSON = withObject "CreatePsbtResponse" $ \obj ->
        CreatePsbtResponse
            <$> (unBase64Encoded <$> obj .: "psbt")
            <*> (toSatoshis <$> obj .: "fee")
            <*> obj .: "changepos"

{- |
 Creates and funds a transaction in the Partially Signed Transaction format.
 Implements the Creator and Updater roles.

 @since 0.3.0.0
-}
createFundedPsbt ::
    -- | Leave empty to add inputs automatically. See add_inputs option.
    [PsbtInput] ->
    -- | The outputs (key-value pairs), where none of the keys are duplicated.
    -- That is, each address can only appear once and there can only be one 'data' object.
    -- For compatibility reasons, a dictionary, which holds the key-value pairs directly, is also
    -- accepted as second parameter.
    PsbtOutputs ->
    -- | Raw locktime. Non-0 value also locktime-activates inputs
    Maybe Int ->
    Maybe CreatePsbtOptions ->
    -- | Include BIP 32 derivation paths for public keys if we know them
    Maybe Bool ->
    BitcoindClient CreatePsbtResponse

-- | @since 0.3.0.0
data ProcessPsbtResponse = ProcessPsbtResponse
    { -- | The base64-encoded partially signed transaction
      processPsbtPsbt :: Text
    , -- | If the transaction has a complete set of signatures
      processPsbtComplete :: Bool
    }
    deriving (Eq, Show)

instance FromJSON ProcessPsbtResponse where
    parseJSON = withObject "ProcessPsbtResponse" $ \obj ->
        ProcessPsbtResponse <$> obj .: "psbt" <*> obj .: "complete"

{- | Removes the wallet encryption key from memory, locking the wallet.  After
 calling this method, you will need to call walletpassphrase again before being
 able to call any methods which require the wallet to be unlocked.

 @since 0.3.0.0
-}
walletLock :: BitcoindClient ()

{- | Stores the wallet decryption key in memory for 'timeout' seconds.  This is
 needed prior to performing transactions related to private keys such as sending
 bitcoins

 Note: Issuing the walletpassphrase command while the wallet is already unlocked
 will set a new unlock time that overrides the old one.

 @since 0.3.0.0
-}
walletPassphrase ::
    -- | The wallet passphrase
    Text ->
    -- | The time to keep the decryption key in seconds; capped at 100000000 (~3 years).
    Int ->
    BitcoindClient ()

{- | Update a PSBT with input information from our wallet and then sign inputs
 that we can sign for. Requires wallet passphrase to be set with walletpassphrase
 call if wallet is encrypted.

 @since 0.3.0.0
-}
processPsbt ::
    -- | The transaction base64 string
    Text ->
    -- | Also sign the transaction when updating
    Maybe Bool ->
    -- | The signature hash type to sign with if not specified by the PSBT. Must
    -- be one of "ALL" "NONE" "SINGLE" "ALL|ANYONECANPAY" "NONE|ANYONECANPAY"
    -- "SINGLE|ANYONECANPAY"
    Maybe Text ->
    -- | Include BIP 32 derivation paths for public keys if we know them
    Maybe Bool ->
    BitcoindClient ProcessPsbtResponse
processPsbt psbt sign = processPsbt_ psbt sign . fromMaybe "ALL"

processPsbt_ ::
    Text ->
    Maybe Bool ->
    Text ->
    Maybe Bool ->
    BitcoindClient ProcessPsbtResponse
