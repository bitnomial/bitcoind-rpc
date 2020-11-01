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

`bitcoind ==0.19.0.1`



Test suite
----

The test suite is in `bitcoind-regtest`, and only runs on Linux.


Development
----

```sh
git clone https://github.com/bitnomial/bitcoind-rpc
cd bitcoind-rpc
cabal build all && cabal test all
```

Please use `ormolu` or `fourmolu` (with the default configuration) for code formatting.
