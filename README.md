bitcoind-rpc
====

This is a Haskell interface to the bitcoin-core daemon RPC.  It expresses
parameters and return values with types from [haskoin-core][1] where possible.
Not all RPC methods are supported, but there are enough to be useful for many
applications.

[1]: https://github.com/haskoin/haskoin-core


Supported bitcoin-core versions
----

`bitcoind ==0.19.0.1`


Test suite
----

The test suite is non portable and only runs on Linux.
