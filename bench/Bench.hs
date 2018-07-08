
module Main where

import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
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
    , bgroup "recursive deeply-nested data structure" (fmap nestTest [22..25])
    ]

data ExampleExpr
  = A
  | B ExampleExpr
  | C [ExampleExpr]
  deriving (Show)

nest :: ExampleExpr -> Int -> ExampleExpr
nest expr 0 = expr
nest expr n = nest (B expr) (n - 1)

-- | There was a bug in the pretty-simple code that caused deeply nested data
-- structures to have an exponential runtime.  Effectively, the runtime doubled
-- at level.  The following benchmark is to make sure that we don't
-- accidentally introduce this exponential runtime again.
nestTest :: Int -> Benchmark
nestTest n = bench ("level " <> show n) $ nf test n
  where
    test :: Int -> Text
    test = pShow . nest (C [A,A])
