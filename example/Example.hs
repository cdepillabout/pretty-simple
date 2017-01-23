
{- |
Copyright   :  Dennis Gosnell 2017
License     :  BSD3
Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This is an short example of using 'pPrint' from "Text.Pretty.Simple" to
pretty-print a Haskell data type.
-}
module Main where

import Text.Pretty.Simple (pPrint)

import Example.Data (bar)

main :: IO ()
main = do
  putStrLn "\nThe following normal \"print\" output:\n"
  print bar
  putStrLn "\ngets turned into this (using \"Text.Pretty.Simple.pPrint\"):\n"
  pPrint bar
