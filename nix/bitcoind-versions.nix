{
  v0-19-1 = builtins.fetchTarball {
    url = "https://bitcoincore.org/bin/bitcoin-core-0.19.1/bitcoin-0.19.1-x86_64-linux-gnu.tar.gz";
    sha256 = "032fz20d0nhc07gcharidmzbz3y871gk4i1lvv7yrh5nkz7pqxpq";
  };

  v0-20-0 = builtins.fetchTarball {
    url = "https://bitcoincore.org/bin/bitcoin-core-0.20.0/bitcoin-0.20.0-x86_64-linux-gnu.tar.gz";
    sha256 = "02rizar9h2frip8m4lamdvjag18kb0blsnpckdy28mgvcck3z6ab";
  };

  v0-20-1 = builtins.fetchTarball {
    url = "https://bitcoincore.org/bin/bitcoin-core-0.20.1/bitcoin-0.20.1-x86_64-linux-gnu.tar.gz";
    sha256 = "0rh4x2gvihgrsz7rb6z4iq6xsm7pmiyrrs03adyadfim6yac786h";
  };

  v0-21-0 = builtins.fetchTarball {
    url = "https://bitcoincore.org/bin/bitcoin-core-0.21.0/bitcoin-0.21.0-x86_64-linux-gnu.tar.gz";
    sha256 = "1kfx4wbigrgiwx3s7609wp4g371b63r1hvij83h25i1j3dm4wijv";
  };
}
