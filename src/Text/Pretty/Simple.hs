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
  , defaultOutputOptions
  , noColorOutputOptions
  -- * Examples
  -- $examples
  ) where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (toList)
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.IO as LText

import Text.Pretty.Simple.Internal
       (OutputOptions(..), UseColor(..), defaultOutputOptions,
        expressionParse, expressionsToOutputs, render)

------------------------------
-- normal (color) functions --
------------------------------

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt defaultOutputOptions

pShow :: Show a => a -> Text
pShow = pShowOpt defaultOutputOptions

pString :: String -> Text
pString = pStringOpt defaultOutputOptions

------------------------
-- no-color functions --
------------------------

pPrintNoColor :: (MonadIO m, Show a) => a -> m ()
pPrintNoColor = pPrintOpt noColorOutputOptions

pShowNoColor :: Show a => a -> Text
pShowNoColor = pShowOpt noColorOutputOptions

pStringNoColor :: String -> Text
pStringNoColor = pStringOpt noColorOutputOptions

noColorOutputOptions :: OutputOptions
noColorOutputOptions = defaultOutputOptions {_useColor = NoColor}

---------------------------------
-- functions that take options --
---------------------------------

pPrintOpt :: (MonadIO m, Show a) => OutputOptions -> a -> m ()
pPrintOpt outputOptions = liftIO . LText.putStrLn . pShowOpt outputOptions

pShowOpt :: Show a => OutputOptions -> a -> Text
pShowOpt outputOptions = pStringOpt outputOptions . show

pStringOpt :: OutputOptions -> String -> Text
pStringOpt outputOptions string =
  case expressionParse string of
    Left _ -> pack string
    Right expressions ->
      render outputOptions . toList $ expressionsToOutputs expressions

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
