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
* Autominer to generate blocks with many transactions


Supported bitcoin-core versions
----

Full support only for `v22.0`.

Limited support for:

* `v0.19.1`
* `v0.20.0`
* `v0.20.1`
* `v0.21.0`
* `v0.21.1`
* `v0.21.0` 
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

Known issues
----

- A bug in `servant-client` regarding the mishandling of empty query string
  parameters causes bad requests to be sent to bitcoin's RPC server using this
  library; the issue has since been fixed upstream in servant by
  https://github.com/haskell-servant/servant/pull/1589, however the fix is
  only released on `servant-client-0.20` or newer, if you need a build plan
  where only `servant-client-0.19` is available (e.g. stackage LTS <21.22)
  make sure to include that fix, or pin
  `58aa0d1c0c19f7b1c26ffc52bfd65c70934704c9` which is the latest release on
  `servant-0.19.*` series that's known to work reliably. Most likely if using
  GHC 9.6 or greater there's no need to worry about this.
