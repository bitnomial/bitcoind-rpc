{
  v25-2 = builtins.fetchTarball {
    url = "https://bitcoincore.org/bin/bitcoin-core-25.2/bitcoin-25.2-x86_64-linux-gnu.tar.gz";
    sha256 = "16a4lpvk19m6bmys4zlyh0q5wx83q7v2whjjdb32x0shgzpzvcmy";
  };
  v26-1 = builtins.fetchTarball {
    url = "https://bitcoincore.org/bin/bitcoin-core-26.1/bitcoin-26.1-x86_64-linux-gnu.tar.gz";
    sha256 = "1brxbss4xkhnh5fzp32dgwjwvz0y3y6sn9pa9w48s51p43m7n917";
  };
  v27-0 = builtins.fetchTarball {
    url = "https://bitcoincore.org/bin/bitcoin-core-27.0/bitcoin-27.0-x86_64-linux-gnu.tar.gz";
    sha256 = "05i4zrdwr2rnbimf4fmklbm1mrvxg1bnv3yrrx44cp66ba0nd3jg";
  };
}
