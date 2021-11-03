# Revision history for bitcoind-regtest

## 0.3.0.0 -- 2021-??-??

* Adds logic to generate blocks with trannsactions (for fee estimation) and an
  exec to run this logic
* Adds a function to start up a cluster of connected nodes
* Changes to `NodeHandle` (incl. adding bitcoind version)
* Enable wallets 

## 0.2.0.0 -- 2020-11-01 

* Create zmq sockets for block and transaction subscriptions to the ephemeral node

## 0.1.0.0 -- 2020-03-07

Initial release
