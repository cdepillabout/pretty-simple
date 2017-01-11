
module Main where

import Text.Pretty.Simple (pPrint)

import Example.Data (baz)

main :: IO ()
main = pPrint baz
