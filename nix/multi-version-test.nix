let pkgs = import <nixpkgs> {};
    versions = import ./bitcoind-versions.nix;

    test = bitcoinInstance: pkgs.writeScript "cabal-test"
      ''
      export PATH=${bitcoinInstance}/bin:$PATH
      bitcoind -version | head -n 1
      cabal test all
      '';

    testVersions = builtins.attrValues versions;

    testInstance = v: toString (test v);

    testMany = pkgs.writeScript "test-many"
      ''
      set -e

      separator () {
        echo -e "\n\n~~~~~~~~~~\n\n"
      }

      ${builtins.concatStringsSep "; separator; " (map testInstance testVersions)}

      '';

in testMany
