{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Tasty (defaultMain, testGroup)

import Bitcoin.Core.Regtest (withBitcoind)
import Bitcoin.Core.Test.Generator (testGenerator)
import Bitcoin.Core.Test.Misc (miscRPC)
import Bitcoin.Core.Test.PSBT (psbtRPC)
import Bitcoin.Core.Test.Wallet (walletRPC)

main :: IO ()
main = withBitcoind 8449 Nothing $ \nodeHandle -> do
    threadDelay 1_000_000
    mgr <- newManager defaultManagerSettings
    defaultMain $
        testGroup
            "bitcoind-rpc"
            [ miscRPC mgr nodeHandle
            , walletRPC mgr nodeHandle
            , psbtRPC mgr nodeHandle
            , testGenerator mgr
            ]
