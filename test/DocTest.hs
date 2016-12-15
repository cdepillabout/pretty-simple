
module Main (main) where

import Prelude

import Data.Monoid ((<>))
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doDocTest

doDocTest :: [String] -> IO ()
doDocTest options = doctest $ options <> ghcExtensions

ghcExtensions :: [String]
ghcExtensions =
    [
    --   "-XConstraintKinds"
    -- , "-XDataKinds"
      "-XDeriveDataTypeable"
    , "-XDeriveGeneric"
    -- , "-XEmptyDataDecls"
    , "-XFlexibleContexts"
    -- , "-XFlexibleInstances"
    -- , "-XGADTs"
    -- , "-XGeneralizedNewtypeDeriving"
    -- , "-XInstanceSigs"
    -- , "-XMultiParamTypeClasses"
    -- , "-XNoImplicitPrelude"
    , "-XOverloadedStrings"
    -- , "-XPolyKinds"
    -- , "-XRankNTypes"
    -- , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    -- , "-XStandaloneDeriving"
    -- , "-XTupleSections"
    -- , "-XTypeFamilies"
    -- , "-XTypeOperators"
    ]
