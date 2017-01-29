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

This module contains the functions 'pPrint', 'pShow', and 'pString' for
pretty-printing any Haskell data type with a 'Show' instance.

'pPrint' is the main go-to function when debugging Haskell code.  'pShow' and
'pString' are slight variations on 'pPrint'.

The other variations of 'pPrint', 'pShow', and 'pString' are for printing
without color and changing the indentation amount.  Most users can ignore these.

See the Examples section at the end of this module for examples of acutally
using 'pPrint'.
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

-- | Pretty-print any data type that has a 'Show' instance.
--
-- If you've never seen 'MonadIO' before, you can think of this function as
-- having the following type signature:
--
-- @
--  pPrint :: Show a => a -> IO ()
-- @
pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt defaultOutputOptions

-- | Similar to 'pPrint', but just return the resulting pretty-printed data
-- type as a 'Text' instead of printing it to the screen.
pShow :: Show a => a -> Text
pShow = pShowOpt defaultOutputOptions

-- | Similar to 'pShow', but the first argument is a 'String' representing a
-- data type that has already been 'show'ed.
--
-- This will work on any 'String' that is similar to a Haskell data type.  The
-- only requirement is that the strings are quoted, and braces, parentheses, and
-- brackets are correctly used to represent indentation.  For example,
-- 'pString' will correctly pretty-print JSON.
pString :: String -> Text
pString = pStringOpt defaultOutputOptions

------------------------
-- no-color functions --
------------------------

-- | Similar to 'pPrint', but doesn't print in color.  However, data types
-- will still be indented nicely.
--
-- >>> pPrintNoColor $ Just ["hello", "bye"]
-- Just
--     [ "hello"
--     , "bye"
--     ]
pPrintNoColor :: (MonadIO m, Show a) => a -> m ()
pPrintNoColor = pPrintOpt noColorOutputOptions

-- | Like 'pShow', but without color.
pShowNoColor :: Show a => a -> Text
pShowNoColor = pShowOpt noColorOutputOptions

-- | LIke 'pString', but without color.
pStringNoColor :: String -> Text
pStringNoColor = pStringOpt noColorOutputOptions

-- | 'noColorOutputOptions' is just like 'defaultOutputOptions', but
-- 'outputOptionsUseColor' is set to 'NoColor'.
noColorOutputOptions :: OutputOptions
noColorOutputOptions = defaultOutputOptions {outputOptionsColorOptions = Nothing}

---------------------------------
-- functions that take options --
---------------------------------

-- | Similar to 'pPrint' but takes 'OutputOptions' to change how the
-- pretty-printing is done.
--
-- For example, 'pPrintOpt' can be used to make the indentation much smaller
-- than normal.
--
-- This is what the normal indentation looks like:
--
-- >>> pPrintOpt noColorOutputOptions $ Just ("hello", "bye")
-- Just
--     ( "hello"
--     , "bye"
--     )
--
-- This is what smaller indentation looks like:
--
-- >>> let smallIndent = noColorOutputOptions {outputOptionsIndentAmount = 1}
-- >>> pPrintOpt smallIndent $ Just ("hello", "bye")
-- Just
--  ( "hello"
--  , "bye"
--  )
pPrintOpt :: (MonadIO m, Show a) => OutputOptions -> a -> m ()
pPrintOpt outputOptions = liftIO . LText.putStrLn . pShowOpt outputOptions

-- | Like 'pShow' but takes 'OutputOptions' to change how the
-- pretty-printing is done.
pShowOpt :: Show a => OutputOptions -> a -> Text
pShowOpt outputOptions = pStringOpt outputOptions . show

-- | Like 'pString' but takes 'OutputOptions' to change how the
-- pretty-printing is done.
pStringOpt :: OutputOptions -> String -> Text
pStringOpt outputOptions string =
  case expressionParse string of
    Left _ -> pack string
    Right expressions ->
      render outputOptions . toList $ expressionsToOutputs expressions

-- $examples
--
-- Here are some examples of using 'pPrint' on different data types.  You can
-- look at these examples to get an idea of what 'pPrint' will output.
--
-- The following examples are all using 'pPrintNoColor' instead of 'pPrint'
-- because their output is being checked using
-- <https://github.com/sol/doctest#readme doctest>.  'pPrint' outputs ANSI
-- escape codes in order to produce color, so the following examples would be
-- hard to read had 'pPrint' been used.
--
-- __Simple Haskell data type__
--
-- >>> data Foo a = Foo a String deriving Show
--
-- >>> pPrintNoColor $ Foo 3 "hello"
-- Foo 3 "hello"
--
-- __List__
--
-- >>> pPrintNoColor $ [1,2,3]
-- [ 1
-- , 2
-- , 3
-- ]
--
-- __Slightly more complicated list__
--
-- >>> pPrintNoColor $ [ Foo [ (), () ] "hello" ]
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
-- __Record__
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
-- __Newtype__
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
