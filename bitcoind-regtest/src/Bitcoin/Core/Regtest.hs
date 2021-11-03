module Bitcoin.Core.Regtest (
    -- * Run an ephemeral regtest node
    Version,
    NodeHandle (..),
    portsInUse,
    runBitcoind,
    withBitcoind,
    peerWith,
    withBitcoindCluster,

    -- * Funding
    oneBitcoin,
    createOutput,
    generate,
    spendPackageOutputs,

    -- * Simulation
    generateWithTransactions,

    -- * Internal wallet

    --
    -- In the following lists, entries correspond to each other e.g. the p2pkh
    -- address for @keys !! 1@ is @addrs !! 1@.
    xprv,
    keys,
    pubKeys,
    addrs,
    textAddrs,

    -- * Versions
    v19_1,
    v20_0,
    v20_1,
    v21_0,
    v21_1,
    v22_0,
) where

import Bitcoin.Core.Regtest.Framework
import Bitcoin.Core.Regtest.Generator
