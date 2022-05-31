{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

'pPrint' and 'pPrintLightBg' will intelligently decide whether or not to use
ANSI escape codes for coloring depending on whether or not the output is
a TTY.  This works in most cases.  If you want to force color output,
you can use the 'pPrintForceColor' or 'pPrintForceColorLightBg' functions.

The variations 'pPrintOpt', 'pShowOpt', and 'pStringOpt' are used when
specifying the 'OutputOptions'.  Most users can ignore these.

There are a few other functions available that are similar to 'pPrint'.

See the Examples section at the end of this module for examples of acutally
using 'pPrint'.  See the
<https://github.com/cdepillabout/pretty-simple#textprettysimple README.md>
for examples of printing in color.
-}
module Text.Pretty.Simple
  (
  -- * Output with color on dark background
    pPrint
  , pHPrint
  , pPrintString
  , pHPrintString
  , pPrintForceColor
  , pHPrintForceColor
  , pPrintStringForceColor
  , pHPrintStringForceColor
  , pShow
  , pString
  -- * Aliases for output with color on dark background
  , pPrintDarkBg
  , pHPrintDarkBg
  , pPrintStringDarkBg
  , pHPrintStringDarkBg
  , pPrintForceColorDarkBg
  , pHPrintForceColorDarkBg
  , pPrintStringForceColorDarkBg
  , pHPrintStringForceColorDarkBg
  , pShowDarkBg
  , pStringDarkBg
  -- * Output with color on light background
  , pPrintLightBg
  , pHPrintLightBg
  , pPrintStringLightBg
  , pHPrintStringLightBg
  , pPrintForceColorLightBg
  , pHPrintForceColorLightBg
  , pPrintStringForceColorLightBg
  , pHPrintStringForceColorLightBg
  , pShowLightBg
  , pStringLightBg
  -- * Output with NO color
  , pPrintNoColor
  , pHPrintNoColor
  , pPrintStringNoColor
  , pHPrintStringNoColor
  , pShowNoColor
  , pStringNoColor
  -- * Output With 'OutputOptions'
  , pPrintOpt
  , pHPrintOpt
  , pPrintStringOpt
  , pHPrintStringOpt
  , pShowOpt
  , pStringOpt
  -- * 'OutputOptions'
  , OutputOptions(..)
  , StringOutputStyle(..)
  , defaultOutputOptionsDarkBg
  , defaultOutputOptionsLightBg
  , defaultOutputOptionsNoColor
  , CheckColorTty(..)
  -- * 'ColorOptions'
  , defaultColorOptionsDarkBg
  , defaultColorOptionsLightBg
  , ColorOptions(..)
  , Style(..)
  , Color(..)
  , Intensity(..)
  , colorNull
  -- * Examples
  -- $examples
  ) where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text.Lazy (Text)
import Prettyprinter (SimpleDocStream)
import Prettyprinter.Render.Terminal
      (Color (..), Intensity(Vivid,Dull), AnsiStyle,
       renderLazy, renderIO)
import System.IO (Handle, stdout)

import Text.Pretty.Simple.Internal
       (ColorOptions(..), Style(..), CheckColorTty(..),
        OutputOptions(..), StringOutputStyle(..),
        convertStyle, colorNull,
        defaultColorOptionsDarkBg, defaultColorOptionsLightBg,
        defaultOutputOptionsDarkBg, defaultOutputOptionsLightBg,
        defaultOutputOptionsNoColor, hCheckTTY, layoutString)

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
-- This function is for printing to a dark background.  Use 'pPrintLightBg' for
-- printing to a terminal with a light background.  Different colors are used.
--
-- Prints to 'stdout'.  Use 'pHPrint' to print to a different 'Handle'.
--
-- >>> pPrint [Just (1, "hello")]
-- [ Just
--     ( 1
--     , "hello"
--     )
-- ]
pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pPrint', but take a 'Handle' to print to.
--
-- >>> pHPrint stdout [Just (1, "hello")]
-- [ Just
--     ( 1
--     , "hello"
--     )
-- ]
pHPrint :: (MonadIO m, Show a) => Handle -> a -> m ()
pHPrint = pHPrintOpt CheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pPrint', but the first argument is a 'String' representing a
-- data type that has already been 'show'ed.
--
-- >>> pPrintString $ show [ Just (1, "hello"), Nothing ]
-- [ Just
--     ( 1
--     , "hello"
--     )
-- , Nothing
-- ]
pPrintString :: MonadIO m => String -> m ()
pPrintString = pPrintStringOpt CheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pHPrintString', but take a 'Handle' to print to.
--
-- >>> pHPrintString stdout $ show [ Just (1, "hello"), Nothing ]
-- [ Just
--     ( 1
--     , "hello"
--     )
-- , Nothing
-- ]
pHPrintString :: MonadIO m => Handle -> String -> m ()
pHPrintString = pHPrintStringOpt CheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pPrint', but print in color regardless of whether the output
-- goes to a TTY or not.
--
-- See 'pPrint' for an example of how to use this function.
pPrintForceColor :: (MonadIO m, Show a) => a -> m ()
pPrintForceColor = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pPrintForceColor', but take a 'Handle' to print to.
--
-- See 'pHPrint' for an example of how to use this function.
pHPrintForceColor :: (MonadIO m, Show a) => Handle -> a -> m ()
pHPrintForceColor = pHPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pPrintString', but print in color regardless of whether the
-- output goes to a TTY or not.
--
-- See 'pPrintString' for an example of how to use this function.
pPrintStringForceColor :: MonadIO m => String -> m ()
pPrintStringForceColor = pPrintStringOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pHPrintString', but print in color regardless of whether the
-- output goes to a TTY or not.
--
-- See 'pHPrintString' for an example of how to use this function.
pHPrintStringForceColor :: MonadIO m => Handle -> String -> m ()
pHPrintStringForceColor = pHPrintStringOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pPrintForceColor', but just return the resulting pretty-printed
-- data type as a 'Text' instead of printing it to the screen.
--
-- This function is for printing to a dark background.
--
-- See 'pShowNoColor' for an example of how to use this function.
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
--
-- See 'pStringNoColor' for an example of how to use this function.
pString :: String -> Text
pString = pStringOpt defaultOutputOptionsDarkBg

--------------------------------------------------------
-- aliases for printing in color to a dark background --
--------------------------------------------------------

-- | Alias for 'pPrint'.
pPrintDarkBg :: (MonadIO m, Show a) => a -> m ()
pPrintDarkBg = pPrint

-- | Alias for 'pHPrint'.
pHPrintDarkBg :: (MonadIO m, Show a) => Handle -> a -> m ()
pHPrintDarkBg = pHPrint

-- | Alias for 'pPrintString'.
pPrintStringDarkBg :: MonadIO m => String -> m ()
pPrintStringDarkBg = pPrintString

-- | Alias for 'pHPrintString'.
pHPrintStringDarkBg :: MonadIO m => Handle -> String -> m ()
pHPrintStringDarkBg = pHPrintString

-- | Alias for 'pPrintForceColor'.
pPrintForceColorDarkBg :: (MonadIO m, Show a) => a -> m ()
pPrintForceColorDarkBg = pPrintForceColor

-- | Alias for 'pHPrintForceColor'.
pHPrintForceColorDarkBg :: (MonadIO m, Show a) => Handle -> a -> m ()
pHPrintForceColorDarkBg = pHPrintForceColor

-- | Alias for 'pPrintStringForceColor'.
pPrintStringForceColorDarkBg :: MonadIO m => String -> m ()
pPrintStringForceColorDarkBg = pPrintStringForceColor

-- | Alias for 'pHPrintStringForceColor'.
pHPrintStringForceColorDarkBg :: MonadIO m => Handle -> String -> m ()
pHPrintStringForceColorDarkBg = pHPrintStringForceColor

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
pPrintLightBg = pPrintOpt CheckColorTty defaultOutputOptionsLightBg

-- | Just like 'pHPrintDarkBg', but for printing to a light background.
pHPrintLightBg :: (MonadIO m, Show a) => Handle -> a -> m ()
pHPrintLightBg = pHPrintOpt CheckColorTty defaultOutputOptionsLightBg

-- | Just like 'pPrintStringDarkBg', but for printing to a light background.
pPrintStringLightBg :: MonadIO m => String -> m ()
pPrintStringLightBg = pPrintStringOpt CheckColorTty defaultOutputOptionsLightBg

-- | Just like 'pHPrintStringDarkBg', but for printing to a light background.
pHPrintStringLightBg :: MonadIO m => Handle -> String -> m ()
pHPrintStringLightBg = pHPrintStringOpt CheckColorTty defaultOutputOptionsLightBg

-- | Just like 'pPrintForceColorDarkBg', but for printing to a light
-- background.
pPrintForceColorLightBg :: (MonadIO m, Show a) => a -> m ()
pPrintForceColorLightBg = pPrintOpt NoCheckColorTty defaultOutputOptionsLightBg

-- | Just like 'pHPrintForceColorDarkBg', but for printing to a light
-- background.
pHPrintForceColorLightBg :: (MonadIO m, Show a) => Handle -> a -> m ()
pHPrintForceColorLightBg = pHPrintOpt NoCheckColorTty defaultOutputOptionsLightBg

-- | Just like 'pPrintStringForceColorDarkBg', but for printing to a light
-- background.
pPrintStringForceColorLightBg :: MonadIO m => String -> m ()
pPrintStringForceColorLightBg = pPrintStringOpt NoCheckColorTty defaultOutputOptionsLightBg

-- | Just like 'pHPrintStringForceColorDarkBg', but for printing to a light
-- background.
pHPrintStringForceColorLightBg :: MonadIO m => Handle -> String -> m ()
pHPrintStringForceColorLightBg = pHPrintStringOpt NoCheckColorTty defaultOutputOptionsLightBg

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
pPrintNoColor = pPrintOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Like 'pPrintNoColor', but take a 'Handle' to determine where to print to.
--
-- >>> pHPrintNoColor stdout $ Just ["hello", "bye"]
-- Just
--     [ "hello"
--     , "bye"
--     ]
pHPrintNoColor :: (MonadIO m, Show a) => Handle -> a -> m ()
pHPrintNoColor = pHPrintOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pPrintString', but doesn't print in color.  However, data types
-- will still be indented nicely.
--
-- >>> pPrintStringNoColor $ show $ Just ["hello", "bye"]
-- Just
--     [ "hello"
--     , "bye"
--     ]
pPrintStringNoColor :: MonadIO m => String -> m ()
pPrintStringNoColor = pPrintStringOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Like 'pPrintStringNoColor', but take a 'Handle' to determine where to print to.
--
-- >>> pHPrintStringNoColor stdout $ show $ Just ["hello", "bye"]
-- Just
--     [ "hello"
--     , "bye"
--     ]
pHPrintStringNoColor :: MonadIO m => Handle -> String -> m ()
pHPrintStringNoColor = pHPrintStringOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Like 'pShow', but without color.
--
-- >>> pShowNoColor [ Nothing, Just (1, "hello") ]
-- "[ Nothing\n, Just\n    ( 1\n    , \"hello\"\n    )\n]"
pShowNoColor :: Show a => a -> Text
pShowNoColor = pShowOpt defaultOutputOptionsNoColor

-- | LIke 'pString', but without color.
--
-- >>> pStringNoColor $ show [1, 2, 3]
-- "[ 1\n, 2\n, 3\n]"
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
-- >>> pPrintOpt NoCheckColorTty defaultOutputOptionsNoColor $ Just ("hello", "bye")
-- Just
--     ( "hello"
--     , "bye"
--     )
--
-- This is what smaller indentation looks like:
--
-- >>> let smallIndent = defaultOutputOptionsNoColor {outputOptionsIndentAmount = 1}
-- >>> pPrintOpt CheckColorTty smallIndent $ Just ("hello", "bye")
-- Just
--  ( "hello"
--  , "bye"
--  )
--
-- Lines in strings get indented
--
-- >>> pPrintOpt NoCheckColorTty defaultOutputOptionsNoColor (1, (2, "foo\nbar\nbaz", 3))
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
-- >>> pPrintOpt CheckColorTty defaultOutputOptionsNoColor (1, (2, Foo, 3))
-- ( 1
-- ,
--     ( 2
--     , foo
--       bar
--       baz
--     , 3
--     )
-- )
--
-- 'CheckColorTty' determines whether to test 'stdout' for whether or not it is
-- connected to a TTY.
--
-- If set to 'NoCheckColorTty', then 'pPrintOpt' won't
-- check if 'stdout' is a TTY.  It will print in color depending on the value
-- of 'outputOptionsColorOptions'.
--
-- If set to 'CheckColorTty', then 'pPrintOpt' will check if 'stdout' is
-- conneted to a TTY.  If 'stdout' is determined to be connected to a TTY, then
-- it will print in color depending on the value of
-- 'outputOptionsColorOptions'.  If 'stdout' is determined to NOT be connected
-- to a TTY, then it will NOT print in color, regardless of the value of
-- 'outputOptionsColorOptions'.
pPrintOpt :: (MonadIO m, Show a) => CheckColorTty -> OutputOptions -> a -> m ()
pPrintOpt checkColorTty outputOptions =
  pHPrintOpt checkColorTty outputOptions stdout

-- | Similar to 'pPrintOpt', but take a 'Handle' to determine where to print
-- to.
pHPrintOpt ::
     (MonadIO m, Show a)
  => CheckColorTty
  -> OutputOptions
  -> Handle
  -> a
  -> m ()
pHPrintOpt checkColorTty outputOptions handle a =
  pHPrintStringOpt checkColorTty outputOptions handle $ show a

-- | Similar to 'pPrintOpt', but the last argument is a string representing a
-- data structure that has already been 'show'ed.
--
-- >>> let foo = show (1, (2, "hello", 3))
-- >>> pPrintStringOpt CheckColorTty defaultOutputOptionsNoColor foo
-- ( 1
-- ,
--     ( 2
--     , "hello"
--     , 3
--     )
-- )
pPrintStringOpt :: MonadIO m => CheckColorTty -> OutputOptions -> String -> m ()
pPrintStringOpt checkColorTty outputOptions =
  pHPrintStringOpt checkColorTty outputOptions stdout

-- | Similar to 'pPrintStringOpt', but take a 'Handle' to determine where to
-- print to.
--
-- >>> let foo = show (1, (2, "hello", 3))
-- >>> pHPrintStringOpt CheckColorTty defaultOutputOptionsNoColor stdout foo
-- ( 1
-- ,
--     ( 2
--     , "hello"
--     , 3
--     )
-- )
pHPrintStringOpt ::
     MonadIO m
  => CheckColorTty
  -> OutputOptions
  -> Handle
  -> String
  -> m ()
pHPrintStringOpt checkColorTty outputOptions handle str = do
  realOutputOpts <-
    case checkColorTty of
      CheckColorTty -> hCheckTTY handle outputOptions
      NoCheckColorTty -> pure outputOptions
  liftIO $ do
    renderIO handle $ layoutStringAnsi realOutputOpts str
    putStrLn ""

-- | Like 'pShow' but takes 'OutputOptions' to change how the
-- pretty-printing is done.
pShowOpt :: Show a => OutputOptions -> a -> Text
pShowOpt outputOptions = pStringOpt outputOptions . show

-- | Like 'pString' but takes 'OutputOptions' to change how the
-- pretty-printing is done.
pStringOpt :: OutputOptions -> String -> Text
pStringOpt outputOptions = renderLazy . layoutStringAnsi outputOptions

layoutStringAnsi :: OutputOptions -> String -> SimpleDocStream AnsiStyle
layoutStringAnsi opts = fmap convertStyle . layoutString opts

-- $examples
--
-- Here are some examples of using 'pPrint' on different data types.  You can
-- look at these examples to get an idea of what 'pPrint' will output.
--
-- __Simple Haskell data type__
--
-- >>> data Foo a = Foo a String Char deriving Show
--
-- >>> pPrint $ Foo 3 "hello" 'a'
-- Foo 3 "hello" 'a'
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
-- >>> pPrint $ [ Foo [ (), () ] "hello" 'b' ]
-- [ Foo
--     [ ()
--     , ()
--     ] "hello" 'b'
-- ]
--
-- >>> pPrint $ [ Foo [ "bar", "baz" ] "hello" 'a', Foo [] "bye" 'b' ]
-- [ Foo
--     [ "bar"
--     , "baz"
--     ] "hello" 'a'
-- , Foo [] "bye" 'b'
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
-- >>> pPrint $ Bar 1 [10, 11] [Foo 1.1 "" 'a', Foo 2.2 "hello" 'b']
-- Bar
--     { barInt = 1
--     , barA =
--         [ 10
--         , 11
--         ]
--     , barList =
--         [ Foo 1.1 "" 'a'
--         , Foo 2.2 "hello" 'b'
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
-- __Compactness options__
--
-- >>> pPrintStringOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = True} "AST [] [Def ((3,1),(5,30)) (Id \"fact'\" \"fact'\") [] (Forall ((3,9),(3,26)) [((Id \"n\" \"n_0\"),KPromote (TyCon (Id \"Nat\" \"Nat\")))])]"
-- AST []
--     [ Def
--         ( ( 3, 1 ), ( 5, 30 ) )
--         ( Id "fact'" "fact'" ) []
--         ( Forall
--             ( ( 3, 9 ), ( 3, 26 ) )
--             [ ( ( Id "n" "n_0" ), KPromote ( TyCon ( Id "Nat" "Nat" ) ) ) ]
--         )
--     ]
--
-- >>> pPrintOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompactParens = True} $ B ( C [A, A] [B A, B (B (B A))] )
-- B
--     ( C
--         [ A
--         , A ]
--         [ B A
--         , B
--             ( B ( B A ) ) ] )
--
-- >>> pPrintOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = True} $ [("id", 123), ("state", 1), ("pass", 1), ("tested", 100), ("time", 12345)]
-- [
--     ( "id", 123 ),
--     ( "state", 1 ),
--     ( "pass", 1 ),
--     ( "tested", 100 ),
--     ( "time", 12345 )
-- ]
--
-- __Initial indent__
--
-- >>> pPrintOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsInitialIndent = 3} $ B ( B ( B ( B A ) ) )
--    B
--        ( B
--            ( B ( B A ) )
--        )
--
-- __Weird/illegal show instances__
--
-- >>> pPrintString "2019-02-18 20:56:24.265489 UTC"
-- 2019-02-18 20:56:24.265489 UTC
--
-- >>> pPrintString "a7ed86f7-7f2c-4be5-a760-46a3950c2abf"
-- a7ed86f7-7f2c-4be5-a760-46a3950c2abf
--
-- >>> pPrintString "192.168.0.1:8000"
-- 192.168.0.1:8000
--
-- >>> pPrintString "A @\"type\" 1"
-- A @"type" 1
--
-- >>> pPrintString "2+2"
-- 2+2
--
-- >>> pPrintString "1.0e-2"
-- 1.0e-2
--
-- >>> pPrintString "0x1b"
-- 0x1b
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
--
-- If you don't want non-printable characters to be escaped, take a look at
-- 'outputOptionsStringStyle' and 'StringOutputStyle'.
