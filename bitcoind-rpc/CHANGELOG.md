# Revision history for bitcoind-rpc

## 0.3.0.0 -- 2021-??-??

* Add the "Wallet" section of the RPC
* Add RPC commands concerned with PSBTs
* Change handling of optional parameters
* Changes the type used to represent `vout` values
* Changes the binding of "getrawtransaction" to `getRawTransaction` 

## 0.2.0.0 -- 2020-11-01 

* Define our own `BlockHeader` type instead of using the one from `haskoin-core`
* Refactor code to deal with new `aeson` instance definitions in `haskoin-core 0.13.5`
* Adds `getBlockFilter` for fetching BIP 157 filters

## 0.1.0.0 -- 2020-03-07

Initial release
