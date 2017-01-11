
module Main where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Text.Pretty.Simple (pShow)

import Example.Data (foo, bar, baz)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "pShow"
        [ bench "Foo" $ nf pShow foo
        , bench "Bar" $ nf pShow bar
        , bench "Baz" $ nf pShow baz
        ]
    ]
