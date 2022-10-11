
# pretty-simple Web Page

This directory contains an interactive web page that can be compiled with GHCJS
to show how `pretty-simple` works.

This Haskell package can be built with GHCJS with the command `nix-build`.
You'll need Nix [installed](https://nixos.org/download.html) for this to work.
You'll also need to setup the Miso Nix cache, as explained
[here](https://github.com/cdepillabout/pretty-simple/pull/117#issuecomment-1258023974).

This Haskell package can also be built with GHC with the command `cabal build web`.
Running this executable with `cabal run web` will start a web server listening on
`0.0.0.0:8000`.  You should be able to see the web page by opening
<http://localhost:8000> in a web browser.  _Note_ that you will need to run
`cabal run web` within this current directory.
