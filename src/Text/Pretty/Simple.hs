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

'pPrint', 'pShow', and 'pString' will pretty-print in color using ANSI escape
codes.  They look good on a console with a dark (black) background. The
variations 'pPrintLightBg', 'pShowLightBg', and 'pStringLightBg' are for
printing in color to a console with a light (white) background.  The variations
'pPrintNoColor', 'pShowNoColor', and 'pStringNoColor' are for pretty-printing
without using color.

The variations 'pPrintOpt', 'pShowOpt', and 'pStringOpt' are used when
specifying the 'OutputOptions'.  Most users can ignore these.

See the Examples section at the end of this module for examples of acutally
using 'pPrint'.  See the
<https://github.com/cdepillabout/pretty-simple#textprettysimple README.md>
for examples of printing in color.
-}
module Text.Pretty.Simple
  (
  -- * Output with color on dark background
    pPrint
  , pPrintForceColor
  , pShow
  , pString
  -- * Aliases for output with color on dark background
  , pPrintDarkBg
  , pPrintForceColorDarkBg
  , pShowDarkBg
  , pStringDarkBg
  -- * Output with color on light background
  , pPrintLightBg
  , pPrintForceColorLightBg
  , pShowLightBg
  , pStringLightBg
  -- * Output with NO color
  , pPrintNoColor
  , pShowNoColor
  , pStringNoColor
  -- * Output With 'OutputOptions'
  , pPrintOpt
  , pShowOpt
  , pStringOpt
  -- * 'OutputOptions'
  , OutputOptions(..)
  , defaultOutputOptionsDarkBg
  , defaultOutputOptionsLightBg
  , defaultOutputOptionsNoColor
  , checkTTY
  , hCheckTTY
  -- * 'ColorOptions'
  -- $colorOptions
  , defaultColorOptionsDarkBg
  , defaultColorOptionsLightBg
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
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as LText

import Text.Pretty.Simple.Internal
       (OutputOptions(..), defaultColorOptionsDarkBg,
        defaultColorOptionsLightBg, defaultOutputOptionsDarkBg,
        defaultOutputOptionsLightBg, defaultOutputOptionsNoColor,
        checkTTY, hCheckTTY, expressionParse, expressionsToOutputs, render)

-- $setup
-- >>> import Data.Text.Lazy (unpack)

----------------------------------------------------------
-- functions for printing in color to a dark background --
----------------------------------------------------------

-- | Pretty-print any data type that has a 'Show' instance.
--
-- If you've never seen 'MonadIO' before, you can think of this function as
-- having the following type signature:
--
-- @
--  pPrint :: Show a => a -> IO ()
-- @
--
-- This function will only use colors if it detects it's printing to a TTY.
--
-- This function is for printing to a dark background.
pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint v = checkTTY defaultOutputOptionsDarkBg >>= \o -> pPrintOpt o v

-- | Similar to 'pPrint', but print in color regardless of whether the output
-- goes to a TTY or not.
pPrintForceColor :: (MonadIO m, Show a) => a -> m ()
pPrintForceColor = pPrintOpt defaultOutputOptionsDarkBg

-- | Similar to 'pPrintForceColor', but just return the resulting pretty-printed
-- data type as a 'Text' instead of printing it to the screen.
--
-- This function is for printing to a dark background.
pShow :: Show a => a -> Text
pShow = pShowOpt defaultOutputOptionsDarkBg

-- | Similar to 'pShow', but the first argument is a 'String' representing a
-- data type that has already been 'show'ed.
--
-- This will work on any 'String' that is similar to a Haskell data type.  The
-- only requirement is that the strings are quoted, and braces, parentheses, and
-- brackets are correctly used to represent indentation.  For example,
-- 'pString' will correctly pretty-print JSON.
--
-- This function is for printing to a dark background.
pString :: String -> Text
pString = pStringOpt defaultOutputOptionsDarkBg

--------------------------------------------------------
-- aliases for printing in color to a dark background --
--------------------------------------------------------

-- | Alias for 'pPrint'.
pPrintDarkBg :: (MonadIO m, Show a) => a -> m ()
pPrintDarkBg = pPrint

-- | Alias for 'pPrintForceColor'.
pPrintForceColorDarkBg :: (MonadIO m, Show a) => a -> m ()
pPrintForceColorDarkBg = pPrintForceColor

-- | Alias for 'pShow'.
pShowDarkBg :: Show a => a -> Text
pShowDarkBg = pShow

-- | Alias for 'pString'.
pStringDarkBg :: String -> Text
pStringDarkBg = pString

-----------------------------------------------------------
-- functions for printing in color to a light background --
-----------------------------------------------------------

-- | Just like 'pPrintDarkBg', but for printing to a light background.
pPrintLightBg :: (MonadIO m, Show a) => a -> m ()
pPrintLightBg v = checkTTY defaultOutputOptionsLightBg >>= \o -> pPrintOpt o v

-- | Just like 'pPrintForceColorDarkBg', but for printing to a light
-- background.
pPrintForceColorLightBg :: (MonadIO m, Show a) => a -> m ()
pPrintForceColorLightBg = pPrintOpt defaultOutputOptionsLightBg

-- | Just like 'pShowDarkBg', but for printing to a light background.
pShowLightBg :: Show a => a -> Text
pShowLightBg = pShowOpt defaultOutputOptionsLightBg

-- | Just like 'pStringDarkBg', but for printing to a light background.
pStringLightBg :: String -> Text
pStringLightBg = pStringOpt defaultOutputOptionsLightBg

------------------------------------------
-- functions for printing without color --
------------------------------------------

-- | Similar to 'pPrint', but doesn't print in color.  However, data types
-- will still be indented nicely.
--
-- >>> pPrintNoColor $ Just ["hello", "bye"]
-- Just
--     [ "hello"
--     , "bye"
--     ]
pPrintNoColor :: (MonadIO m, Show a) => a -> m ()
pPrintNoColor = pPrintOpt defaultOutputOptionsNoColor

-- | Like 'pShow', but without color.
pShowNoColor :: Show a => a -> Text
pShowNoColor = pShowOpt defaultOutputOptionsNoColor

-- | LIke 'pString', but without color.
pStringNoColor :: String -> Text
pStringNoColor = pStringOpt defaultOutputOptionsNoColor

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
-- >>> pPrintOpt defaultOutputOptionsNoColor $ Just ("hello", "bye")
-- Just
--     ( "hello"
--     , "bye"
--     )
--
-- This is what smaller indentation looks like:
--
-- >>> let smallIndent = defaultOutputOptionsNoColor {outputOptionsIndentAmount = 1}
-- >>> pPrintOpt smallIndent $ Just ("hello", "bye")
-- Just
--  ( "hello"
--  , "bye"
--  )
--
-- Lines in strings get indented
--
-- >>> pPrintOpt defaultOutputOptionsNoColor (1, (2, "foo\nbar\nbaz", 3))
-- ( 1
-- ,
--     ( 2
--     , "foo
--       bar
--       baz"
--     , 3
--     )
-- )
--
-- Lines get indented even in custom show instances
--
-- >>> data Foo = Foo
-- >>> instance Show Foo where show _ = "foo\nbar\nbaz"
-- >>> pPrintOpt defaultOutputOptionsNoColor (1, (2, Foo, 3))
-- ( 1
-- ,
--     ( 2
--     , foo
--       bar
--       baz
--     , 3
--     )
-- )

pPrintOpt :: (MonadIO m, Show a) => OutputOptions -> a -> m ()
pPrintOpt outputOptions = liftIO . LText.putStrLn . pShowOpt outputOptions

-- | Like 'pShow' but takes 'OutputOptions' to change how the
-- pretty-printing is done.
pShowOpt :: Show a => OutputOptions -> a -> Text
pShowOpt outputOptions = pStringOpt outputOptions . show

-- | Like 'pString' but takes 'OutputOptions' to change how the
-- pretty-printing is done.
pStringOpt :: OutputOptions -> String -> Text
pStringOpt outputOptions = render outputOptions . toList . expressionsToOutputs . expressionParse 

-- $colorOptions
--
-- Additional settings for color options can be found in
-- "Text.Pretty.Simple.Internal.Color".

-- $examples
--
-- Here are some examples of using 'pPrint' on different data types.  You can
-- look at these examples to get an idea of what 'pPrint' will output.
--
-- __Simple Haskell data type__
--
-- >>> data Foo a = Foo a String deriving Show
--
-- >>> pPrint $ Foo 3 "hello"
-- Foo 3 "hello"
--
-- __List__
--
-- >>> pPrint $ [1,2,3]
-- [ 1
-- , 2
-- , 3
-- ]
--
-- __Slightly more complicated list__
--
-- >>> pPrint $ [ Foo [ (), () ] "hello" ]
-- [ Foo
--     [ ()
--     , ()
--     ] "hello"
-- ]
--
-- >>> pPrint $ [ Foo [ "bar", "baz" ] "hello", Foo [] "bye" ]
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
-- >>> pPrint $ Bar 1 [10, 11] [Foo 1.1 "", Foo 2.2 "hello"]
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
-- >>> pPrint $ Baz ["hello", "bye"]
-- Baz
--     { unBaz =
--         [ "hello"
--         , "bye"
--         ]
--     }
--
-- __Newline Rules__
--
-- >>> data Foo = A | B Foo | C [Foo] [Foo] deriving Show
--
-- >>> pPrint $ B ( B A )
-- B ( B A )
--
-- >>> pPrint $ B ( B ( B A ) )
-- B
--     ( B ( B A ) )
--
-- >>> pPrint $ B ( B ( B ( B A ) ) )
-- B
--     ( B
--         ( B ( B A ) )
--     )
--
-- >>> pPrint $ B ( C [A, A] [B A, B (B (B A))] )
-- B
--     ( C
--         [ A
--         , A
--         ]
--         [ B A
--         , B
--             ( B ( B A ) )
--         ]
--     )
--
-- __Laziness__
--
-- >>> take 100 . unpack . pShowNoColor $ [1..]
-- "[ 1\n, 2\n, 3\n, 4\n, 5\n, 6\n, 7\n, 8\n, 9\n, 10\n, 11\n, 12\n, 13\n, 14\n, 15\n, 16\n, 17\n, 18\n, 19\n, 20\n, 21\n, 22"
--
-- __Unicode__
--
-- >>> pPrint $ Baz ["猫", "犬", "ヤギ"]
-- Baz
--     { unBaz =
--         [ "猫"
--         , "犬"
--         , "ヤギ"
--         ]
--     }
--
-- __Other__
--
-- Making sure the spacing after a string is correct.
--
-- >>> data Foo = Foo String Int deriving Show
--
-- >>> pPrint $ Foo "bar" 0
-- Foo "bar" 0
--
-- Non-printable characters will get escaped.
--
-- >>> pPrint "this string has non-printable characters: \x8 and \x9"
-- "this string has non-printable characters: \x8 and \x9"
