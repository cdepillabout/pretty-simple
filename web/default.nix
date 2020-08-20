with (import (builtins.fetchTarball {
  url =
    "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) { }); {
  release = let
    hp = pkgs.haskell.packages.ghcjs86.override {
      all-cabal-hashes = builtins.fetchurl {
        url =
          "https://github.com/commercialhaskell/all-cabal-hashes/archive/ead1bd926a1b10b04a5c07c8f15827091fa98b38.tar.gz";
        sha256 = "15i7ia241wb3s9f6l9n2bqldb4ii73xrj49rfr02q43iqbmdjddv";
      };
    };
    prettyprinter = hp.callHackage "prettyprinter" "1.7.0" { };
    prettyprinter-ansi-terminal =
      hp.callHackage "prettyprinter-ansi-terminal" "1.1.2" {
        prettyprinter = prettyprinter;
      };
  in hp.callCabal2nix "web" ./. {
    prettyprinter = prettyprinter;
    pretty-simple = hp.callCabal2nix "pretty-simple" ./.. {
      prettyprinter = prettyprinter;
      prettyprinter-ansi-terminal = prettyprinter-ansi-terminal;
    };
  };
  inherit pkgs;
}
