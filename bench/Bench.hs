
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
    , bgroup "recursive deeply-nested data structure" (fmap nestTest [17..20])
    ]

data ExampleExpr
  = A
  | B ExampleExpr
  | C [ExampleExpr]
  deriving (Show)

nest :: ExampleExpr -> Int -> ExampleExpr
nest expr 0 = expr
nest expr n = nest (B expr) (n - 1)

-- | The runtime for this doubles every level.  It is effectively O(n^2).  Ideally it should be O(n).
-- If you're interested in fixing this you can find more information at the following link:
-- <https://github.com/cdepillabout/pretty-simple/issues/30>.
nestTest :: Int -> Benchmark
nestTest n = bench ("level " <> show n) $ nf test n
  where
    test :: Int -> Text
    test = pShow . nest (C [A,A])
