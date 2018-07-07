import Criterion.Main
import Text.Pretty.Simple

data Expr = A
          | B Expr
          | C [Expr]
  deriving Show

nest 0 = id
nest n = nest (n-1) . B

test n = bench (show n) $ nf test' n
  where 
    test' = pShowNoColor . flip nest (C [A,A])

main = defaultMain [bgroup "nest" $ map test [1..25]]
