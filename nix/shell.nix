{ pkgs ? import <nixpkgs> {} }:

let bitcoindVersions = import ./bitcoind-versions.nix;
    mkBitcoindShell = bitcoindVersion:
      pkgs.mkShell {
        buildInputs = [
          pkgs.gmp
          pkgs.ncurses
          pkgs.secp256k1
          pkgs.zlib
        ];

        shellHook =
          ''
          export PATH="${bitcoindVersion}/bin:$PATH"

          rpc_user=user
          rpc_pass=password

          start_bitcoind () {
            bitcoind -daemon -regtest -rpcuser=$rpc_user -rpcpassword=$rpc_pass
          }

          alias bcli="bitcoin-cli -regtest -rpcuser=$rpc_user -rpcpassword=$rpc_pass"

          echo "Start a regtest node with 'start_bitcoind'"
          echo "Access the node with 'bcli'"
          '';
      };
in

{
  v0-19-1 = mkBitcoindShell bitcoindVersions.v0-19-1;
  v0-20-0 = mkBitcoindShell bitcoindVersions.v0-20-0;
  v0-20-1 = mkBitcoindShell bitcoindVersions.v0-20-1;
  v0-21-0 = mkBitcoindShell bitcoindVersions.v0-21-0;
  v0-21-1 = mkBitcoindShell bitcoindVersions.v0-21-1;
  v22-0 = mkBitcoindShell bitcoindVersions.v22-0;
  v23-0 = mkBitcoindShell bitcoindVersions.v23-0;
}
