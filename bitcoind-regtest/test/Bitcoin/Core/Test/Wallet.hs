{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bitcoin.Core.Test.Wallet (
    walletRPC,
) where

import Control.Monad (replicateM, replicateM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Haskoin (OutPoint (OutPoint))
import Network.HTTP.Client (Manager)
import System.Directory (removeFile)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, (@?=))

import Bitcoin.Core.RPC (
    AddressType (..),
    BitcoindClient,
    BumpFeeOptions (BumpFeeOptions),
    CreatePsbtOptions (CreatePsbtOptions),
    DescriptorRequest (DescriptorRequest),
    ListUnspentOptions (ListUnspentOptions),
    OutputDetails,
    PrevTx (PrevTx),
    PsbtOutputs (PsbtOutputs),
    Purpose (PurposeRecv),
    withWallet,
 )
import qualified Bitcoin.Core.RPC as RPC
import Bitcoin.Core.Regtest (NodeHandle, Version, nodeVersion, v20_1, v21_1)
import Bitcoin.Core.Test.Utils (
    bitcoindTest,
    createWallet,
    generate,
    initWallet,
    shouldMatch,
    testRpc,
    unlockWallet,
    walletPassword,
 )

walletRPC :: Manager -> NodeHandle -> TestTree
walletRPC mgr h =
    testGroup "wallet-rpc" $
        if v > v20_1
            then
                bitcoindTest mgr h
                    <$> [ testRpc "walletCommands" testWalletCommands
                        , testRpc "addressCommands" testAddressCommands
                        , testRpc "transactionCommands" testTransactionCommands
                        , testRpc "descriptorCommands" $ testDescriptorCommands v
                        , testRpc "psbtCommands" testPsbtCommands
                        ]
            else mempty
  where
    v = nodeVersion h

testWalletCommands :: BitcoindClient ()
testWalletCommands = do
    loadWalletR <- createWallet walletName
    liftIO $ RPC.loadWalletName loadWalletR @?= walletName

    RPC.listWallets >>= shouldMatch [walletName] . filter (not . Text.null)

    RPC.unloadWallet (Just walletName) Nothing
    RPC.listWallets >>= shouldMatch mempty

    RPC.loadWallet walletName Nothing
    RPC.listWallets >>= shouldMatch [walletName]

    walletInfo <- RPC.getWalletInfo
    liftIO $ do
        RPC.walletStateName walletInfo @?= walletName
        RPC.walletStateTxCount walletInfo @?= 0

    RPC.rescanBlockchain (Just 0) (Just 0)
    RPC.abortRescan

    RPC.walletLock
    RPC.walletPassphrase "password" 60

    RPC.setTxFee 1000

    tmpDir <- liftIO getCanonicalTemporaryDirectory
    let walletDump = tmpDir <> "/wallet-dump"
    RPC.dumpWallet walletDump

    let walletBackup = tmpDir <> "/wallet-backup"
    RPC.backupWallet walletBackup

    RPC.walletLock
    liftIO $ mapM_ removeFile [walletDump, walletBackup]
    void $ RPC.unloadWallet (Just walletName) Nothing
  where
    walletName = "testCreateWallet"

testAddressCommands :: BitcoindClient ()
testAddressCommands = do
    mapM_ initWallet [wallet1, wallet2]

    (privKey, someAddress) <- withWallet wallet1 $ do
        newAddress <- RPC.getNewAddress (Just label1) (Just Bech32)
        newAddress2 <- RPC.getNewAddress (Just label2) (Just Legacy)
        newAddress3 <- RPC.getNewAddress (Just label2) (Just P2SHSegwit)
        newAddress4 <- RPC.getNewAddress (Just label2) Nothing

        RPC.listLabels Nothing >>= shouldMatch [label1, label2]
        RPC.getAddressesByLabel label1 >>= shouldMatch [(newAddress, PurposeRecv)] . Map.toList
        RPC.getAddressesByLabel label2
            >>= shouldMatch
                ( sortOn
                    fst
                    [ (newAddress2, PurposeRecv)
                    , (newAddress3, PurposeRecv)
                    , (newAddress4, PurposeRecv)
                    ]
                )
                . Map.toList

        RPC.setLabel newAddress label3
        RPC.getAddressesByLabel label3 >>= shouldMatch [(newAddress, PurposeRecv)] . Map.toList

        RPC.addMultisigAddress 2 [newAddress2, newAddress3, newAddress4] Nothing Nothing
        unlockWallet
        privKey <- RPC.dumpPrivKey newAddress

        signingAddress <- RPC.getNewAddress Nothing (Just Legacy)
        RPC.signMessage signingAddress "TEST"

        pure (privKey, newAddress2)

    withWallet wallet2 $ do
        unlockWallet
        RPC.importPrivKey privKey (Just "priv-test") Nothing
        RPC.importAddress someAddress (Just "addr-test") (Just True) Nothing
  where
    label1 = "account-1"
    label2 = "account-2"
    label3 = "account-3"

    wallet1 = "testAddressCommands1"
    wallet2 = "testAddressCommands2"

testTransactionCommands :: BitcoindClient ()
testTransactionCommands = do
    mapM_ initWallet [userWalletA, minerWallet]

    addressA1 <- withWallet userWalletA $ RPC.getNewAddress (Just labelA1) Nothing

    txId <- withWallet minerWallet $ do
        replicateM_ 200 generate
        unlockWallet
        txId <- sendSimple addressA1 sendAmount1 "funding" "user-a"
        replicateM_ 200 generate
        pure txId

    addrs <- withWallet userWalletA $ do
        RPC.getBalances
        RPC.getBalance Nothing Nothing Nothing >>= shouldMatch sendAmount1
        RPC.listReceivedByAddress Nothing Nothing Nothing Nothing
            >>= shouldMatch 1 . length
        RPC.listReceivedByLabel Nothing Nothing Nothing
            >>= shouldMatch 1 . length
        RPC.getAddressInfo addressA1
        RPC.getRawChangeAddress Nothing
        RPC.getReceivedByAddress addressA1 Nothing >>= shouldMatch sendAmount1
        RPC.getReceivedByLabel labelA1 Nothing >>= shouldMatch sendAmount1
        RPC.getTransaction txId Nothing
        RPC.listSinceBlock Nothing Nothing Nothing Nothing
        RPC.listTransactions Nothing Nothing Nothing Nothing
        replicateM 20 $ RPC.getNewAddress (Just labelA2) Nothing

    withWallet minerWallet $ do
        unspent <-
            RPC.listUnspent
                Nothing
                Nothing
                Nothing
                (Just True)
                (ListUnspentOptions Nothing Nothing Nothing Nothing)
        liftIO . assertBool "At least one output" $ (not . null) unspent
        RPC.lockUnspent False $ toPrevTx <$> unspent
        RPC.listLockUnspent >>= shouldMatch (toOutPoint <$> unspent)

        unlockWallet
        RPC.lockUnspent True $ toPrevTx <$> unspent
        txId2 <-
            RPC.sendMany
                (Map.fromList $ (,100_000) <$> addrs)
                (Just "send-many")
                mempty
                (Just True)
                Nothing
                Nothing
                (Just 1)
        let options =
                BumpFeeOptions
                    { RPC.bumpFeeConfTarget = Nothing
                    , RPC.bumpFeeFeeRate = Just 10
                    , RPC.bumpFeeReplaceable = True
                    , RPC.bumpFeeEstimateMode = Nothing
                    }
        RPC.psbtBumpFee txId2 (Just options)
        RPC.bumpFee txId2 (Just options)
        pure ()
  where
    userWalletA = "testTransactionCommands-A"
    minerWallet = "testTransactionCommands-M"

    labelA1 = "recv-1"
    labelA2 = "recv-2"

    sendAmount1 = 3_0000_0000

    sendSimple addr amount what to =
        RPC.sendToAddress
            addr
            amount
            (Just what)
            (Just to)
            Nothing
            (Just True)
            Nothing
            Nothing
            (Just True)
            (Just 1)

    toPrevTx =
        PrevTx
            <$> RPC.outputTxId
            <*> RPC.outputVOut
            <*> RPC.outputScriptPubKey
            <*> pure Nothing
            <*> pure Nothing
            <*> RPC.outputAmount

toOutPoint :: OutputDetails -> OutPoint
toOutPoint = OutPoint <$> RPC.outputTxId <*> fromIntegral . RPC.outputVOut

testDescriptorCommands :: Version -> BitcoindClient ()
testDescriptorCommands v = do
    RPC.createWallet walletName Nothing Nothing walletPassword (Just True) (Just True) Nothing Nothing
    withWallet walletName $ do
        RPC.walletPassphrase walletPassword 30
        RPC.getNewAddress (Just "internal") Nothing
        RPC.importDescriptors
            [ DescriptorRequest
                theDescriptor
                Nothing
                (Just (0, Just 100))
                Nothing
                Nothing
                Nothing
                (Just "imported-descriptor")
            ]
        when (v > v21_1) $ RPC.listDescriptors >>= shouldMatch 6 . length
  where
    walletName = "descriptorWallet"
    -- Taken from bitcoind descriptor wallet documentation
    theDescriptor = "pkh([d34db33f/44'/0'/0']xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL/1/*)"

testPsbtCommands :: BitcoindClient ()
testPsbtCommands = do
    mapM_ initWallet [walletA, walletB, walletC]

    changeAddr <- withWallet walletA $ do
        replicateM_ 200 generate
        RPC.getNewAddress (Just "change") Nothing

    utxos <- withWallet walletB $ do
        replicateM_ 200 generate
        take 3 . sortOn isConfirmed
            <$> RPC.listUnspent
                Nothing
                Nothing
                Nothing
                Nothing
                (ListUnspentOptions Nothing Nothing Nothing Nothing)

    liftIO $ length utxos @?= 3

    recvAddr <- withWallet walletC $ RPC.getNewAddress Nothing Nothing

    let theAmount = sum (RPC.outputAmount <$> utxos) `quot` 2
        outputs = PsbtOutputs [(recvAddr, theAmount)] mempty
        options =
            CreatePsbtOptions
                { RPC.createPsbtAddInputs = Just True
                , RPC.createPsbtChangeAddress = Just changeAddr
                , RPC.createPsbtChangePosition = Just 1
                , RPC.createPsbtChangeType = Nothing
                , RPC.createPsbtIncludeWatching = Just True
                , RPC.createPsbtLockUnspents = Just True
                , RPC.createPsbtFeeRate = Just 1
                , RPC.createPsbtSubtractFee = mempty
                , RPC.createPsbtReplaceable = Just True
                , RPC.createPsbtConfTarget = Nothing
                , RPC.createPsbtEstimateMode = Nothing
                }

    void . withWallet walletA $ do
        unlockWallet
        RPC.createFundedPsbt mempty outputs Nothing (Just options) (Just True)
  where
    walletA = "psbtWallet-A"
    walletB = "psbtWallet-B"
    walletC = "psbtWallet-C"

    isConfirmed = (> 6) . RPC.outputConfs

-- TODO encryptWallet,
-- TODO importWallet,
-- TODO importMulti,
-- TODO signRawTx,
-- TODO processPbst
