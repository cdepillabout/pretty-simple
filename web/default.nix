with (import
  (builtins.fetchTarball {
    url =
      "https://github.com/dmjio/miso/archive/refs/tags/1.8.3.tar.gz";
    sha256 = "0kcr5agbcynm003zj70yfkhsc169ahdcp9pkyr795p5mc3ykycjl";
  })
{ });
#TODO we can remove all these patches once we're not stuck on such old tools
# unfortunately GHCJS 8.10.7 has serious performance issues: https://github.com/dmjio/miso/pull/693
let
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
  app = hp.callCabal2nix "web" ./. {
    prettyprinter = prettyprinter;
    pretty-simple = hp.callCabal2nix "pretty-simple" ./.. {
      prettyprinter = prettyprinter;
      prettyprinter-ansi-terminal = prettyprinter-ansi-terminal;
    };
  };
in
pkgs.buildEnv {
  name = "pretty-simple-web";
  paths = [
    app
    (pkgs.runCommand "css" { } ''
      mkdir -p $out/bin/web.jsexe
      cp ${./style.css} $out/bin/web.jsexe/style.css
    '')
  ];
}
