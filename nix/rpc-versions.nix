let pkgs = import <nixpkgs> {};
    versions = import ./bitcoind-versions.nix;

    rpcExplorer = bitcoindInstance: outFile: pkgs.writeScript "rpc-explorer"
      ''
      export PATH=${bitcoindInstance}/bin:$PATH
      cabal build --flags rpc-explorer
      cabal run bitcoind-rpc-explorer -- --output ${outFile}
      '';

    mkRpcVersionData = version:
      rpcExplorer (builtins.getAttr version versions) "./rpc-version-${version}.out";

    concatLines = f: xs: builtins.concatStringsSep "\n" (map f xs);

    rpcVersions = pkgs.writeScript "rpc-versions"
      ''
      ${concatLines mkRpcVersionData (builtins.attrNames versions)}
      '';

in rpcVersions
