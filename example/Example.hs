
module Main where

import Text.Pretty.Simple (pPrint)

import Example.Data (bar)

main :: IO ()
main = do
  putStrLn "\nThe following normal \"print\" output:\n"
  print bar
  putStrLn "\ngets turned into this (using \"Text.Pretty.Simple.pPrint\"):\n"
  pPrint bar
