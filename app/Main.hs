module Main where

-- This is a small executable that will pretty-print anything from stdin.
-- It can be installed to `~/.local/bin` if you enable the flag `buildexe` like so:
--
-- @
--   $ stack install pretty-simple-2.0.1.1 --flag pretty-simple:buildexe
-- @
--
-- When you run it, you can paste something you want formatted on stdin, then
-- press @Ctrl-D@.  It will print the formatted version on stdout:
--
-- @
--   $ pretty-simple
--   [(Just 3, Just 4)]
--
--   ^D
--
--   [
--       ( Just 3
--       , Just 4
--       )
--   ]
-- @

import Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Text.Pretty.Simple (pString)

main :: IO ()
main = do
  input <- T.getContents
  let output = pString $ unpack input
  LT.putStr output
