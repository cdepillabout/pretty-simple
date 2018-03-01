module Main where

import Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Text.Pretty.Simple (pString)

main :: IO ()
main = do
  input <- T.getContents
  let output = pString $ unpack input
  LT.putStr output
