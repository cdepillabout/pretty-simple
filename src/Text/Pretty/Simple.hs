{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pretty.Simple
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple
  (
  -- * Output With Color
    pShow
  , pPrint
  , pString
  -- * Output With NO Color
  , pShowNoColor
  , pPrintNoColor
  , pStringNoColor
  -- * Output With Output Options
  , pShowOpt
  , pPrintOpt
  , pStringOpt
  , OutputOptions(..)
  , UseColor(..)
  -- * Examples
  -- $examples
  ) where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Monad.IO.Class (MonadIO, liftIO)

import Text.Pretty.Simple.Internal
       (OutputOptions(..), UseColor(..), defaultOutputOptions,
        expressionParse, expressionsToOutputs, render)

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt defaultOutputOptions

pShow :: Show a => a -> String
pShow = pShowOpt defaultOutputOptions

pString :: String -> String
pString = pStringOpt defaultOutputOptions

pPrintNoColor :: (MonadIO m, Show a) => a -> m ()
pPrintNoColor = pPrintOpt noColorOutputOptions

pShowNoColor :: Show a => a -> String
pShowNoColor = pShowOpt noColorOutputOptions

pStringNoColor :: String -> String
pStringNoColor = pStringOpt noColorOutputOptions

noColorOutputOptions :: OutputOptions
noColorOutputOptions = defaultOutputOptions {_useColor = NoColor}

pPrintOpt :: (MonadIO m, Show a) => OutputOptions -> a -> m ()
pPrintOpt outputOptions = liftIO . putStrLn . pShowOpt outputOptions

pShowOpt :: Show a => OutputOptions -> a -> String
pShowOpt outputOptions = pStringOpt outputOptions . show

pStringOpt :: OutputOptions -> String -> String
pStringOpt outputOptions string =
  case expressionParse string of
    Left _ -> string
    Right expressions ->
      render outputOptions $ expressionsToOutputs expressions

-- $examples
-- Simple Haskell datatype:
--
-- >>> data Foo a = Foo a String deriving Show
--
-- >>> pPrintNoColor $ Foo 3 "hello"
-- Foo 3 "hello"
--
-- Lists:
--
-- >>> pPrintNoColor $ [1,2,3]
-- [ 1
-- , 2
-- , 3
-- ]
--
-- Slightly more complicated lists:
--
-- >>> pPrintNoColor $ [ Foo [ (),    () ] "hello" ]
-- [ Foo
--     [ ()
--     , ()
--     ] "hello"
-- ]
--
-- >>> pPrintNoColor $ [ Foo [ "bar", "baz" ] "hello", Foo [] "bye" ]
-- [ Foo
--     [ "bar"
--     , "baz"
--     ] "hello"
-- , Foo [] "bye"
-- ]
--
-- Record:
--
-- >>> :{
-- data Bar b = Bar
--   { barInt :: Int
--   , barA :: b
--   , barList :: [Foo Double]
--   } deriving Show
-- :}
--
-- >>> pPrintNoColor $ Bar 1 [10, 11] [Foo 1.1 "", Foo 2.2 "hello"]
-- Bar
--     { barInt = 1
--     , barA =
--         [ 10
--         , 11
--         ]
--     , barList =
--         [ Foo 1.1 ""
--         , Foo 2.2 "hello"
--         ]
--     }
--
-- Newtype:
--
-- >>> newtype Baz = Baz { unBaz :: [String] } deriving Show
--
-- >>> pPrintNoColor $ Baz ["hello", "bye"]
-- Baz
--     { unBaz =
--         [ "hello"
--         , "bye"
--         ]
--     }
