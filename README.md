bitcoin-core Haskell integration
====

bitcoind-rpc
----

This is a Haskell interface to the bitcoin-core daemon RPC.  It expresses
parameters and return values with types from [haskoin-core][1] where possible.
Not all RPC methods are supported, but there are enough to be useful for many
applications.

[1]: https://github.com/haskoin/haskoin-core


bitcoind-regtest
----

This package provides some tooling for testing a bitcoin-core integration.  Features include:

* Spin up ephemeral regtest nodes
* Fund outputs in one line
* Simple HD wallet


Supported bitcoin-core versions
----

Non-wallet RPC calls:

* `v0.19.1`
* `v0.20.0`
* `v0.20.1`
* `v0.21.0`
* `v0.21.1`

Wallet and PSBT RPC calls: 

* `v0.21.0` - except where noted
* `v0.21.1`

Test suite
----

The test suite is in `bitcoind-regtest` and only runs on Linux.  There is also a nix derivation to run the test suite against multiple `bitcoind` versions:

``` sh
nix-build nix/multi-version-test.nix
./result
```

To dump a representation of each `bitcoind` RPC version suitable for diffing:
``` sh
nix-build nix/rpc-versions.nix
./result
```


Development
----

```sh
git clone https://github.com/bitnomial/bitcoind-rpc
cd bitcoind-rpc
cabal build all && cabal test all
```

Please use `ormolu` or `fourmolu` (with the default configuration) for code formatting.
