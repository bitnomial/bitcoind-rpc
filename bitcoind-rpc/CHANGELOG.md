# Revision history for bitcoind-rpc

## 0.3.0.0 -- 2021-??-??

* Adds RPC commands
    * Wallet section of the RPC
    * Some commands concerned with PSBTs
    * "getdescriptorinfo"
    * "importpubkey"
    * "estimatesmartfee"
* Improves "importmulti"
* Change handling of optional parameters
* Changes the type used to represent `vout` values
* Changes the binding of "getrawtransaction" to `getRawTransaction` 
* Changes fields of `BlockHeader`
* Removes some deprecated (in v0.21) fields

## 0.2.0.0 -- 2020-11-01 

* Define our own `BlockHeader` type instead of using the one from `haskoin-core`
* Refactor code to deal with new `aeson` instance definitions in `haskoin-core 0.13.5`
* Adds `getBlockFilter` for fetching BIP 157 filters

## 0.1.0.0 -- 2020-03-07

Initial release
