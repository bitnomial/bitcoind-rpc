module Bitcoin.Core.Regtest (
    -- * Run an ephemeral regtest node
    Version,
    NodeHandle (..),
    portsInUse,
    runBitcoind,
    withBitcoind,
    peerWith,
    dumpCommandLine,

    -- * Funding
    oneBitcoin,
    createOutput,
    generate,
    spendPackageOutputs,

    -- * Simulation
    GeneratorConfig (..),
    GeneratorStatus (..),
    GeneratorState (..),
    GeneratorHandle (..),
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
    v25_2,
    v26_1,
    v27_0,
) where

import Bitcoin.Core.Regtest.Framework
import Bitcoin.Core.Regtest.Generator
