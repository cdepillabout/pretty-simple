{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pretty.Simple.Internal.OutputPrinter
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple.Internal.Color
  where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.Console.ANSI
       (Color(..), ColorIntensity(..), ConsoleIntensity(..),
        ConsoleLayer(..), SGR(..), setSGRCode)

-- | These options are for colorizing the output of functions like 'pPrint'.
--
-- For example, if you set 'colorQuote' to something like 'colorVividBlueBold',
-- then the quote character (@\"@) will be output as bright blue in bold.
--
-- If you don't want to use a color for one of the options, use 'colorNull'.
data ColorOptions = ColorOptions
  { colorQuote :: Builder
  -- ^ Color to use for quote characters (@\"@) around strings.
  , colorString :: Builder
  -- ^ Color to use for strings.
  , colorError :: Builder
  -- ^ (currently not used)
  , colorNum :: Builder
  -- ^ (currently not used)
  , colorRainbowParens :: [Builder]
  -- ^ A list of 'Builder' colors to use for rainbow parenthesis output.  Use
  -- '[]' if you don't want rainbow parenthesis.  Use just a single item if you
  -- want all the rainbow parenthesis to be colored the same.
  } deriving (Eq, Generic, Show, Typeable)

------------------------------------
-- Dark background default colors --
------------------------------------

-- | Default color options for use on a dark background.
--
-- 'colorQuote' is 'defaultColorQuoteDarkBg'. 'colorString' is
-- 'defaultColorStringDarkBg'.  'colorError' is 'defaultColorErrorDarkBg'.
-- 'colorNum' is 'defaultColorNumDarkBg'.  'colorRainbowParens' is
-- 'defaultColorRainboxParensDarkBg'.
defaultColorOptionsDarkBg :: ColorOptions
defaultColorOptionsDarkBg =
  ColorOptions
  { colorQuote = defaultColorQuoteDarkBg
  , colorString = defaultColorStringDarkBg
  , colorError = defaultColorErrorDarkBg
  , colorNum = defaultColorNumDarkBg
  , colorRainbowParens = defaultColorRainbowParensDarkBg
  }

-- | Default color for 'colorQuote' for dark backgrounds. This is
-- 'colorVividWhiteBold'.
defaultColorQuoteDarkBg :: Builder
defaultColorQuoteDarkBg = colorVividWhiteBold

-- | Default color for 'colorString' for dark backgrounds. This is
-- 'colorVividBlueBold'.
defaultColorStringDarkBg :: Builder
defaultColorStringDarkBg = colorVividBlueBold

-- | Default color for 'colorError' for dark backgrounds.  This is
-- 'colorVividRedBold'.
defaultColorErrorDarkBg :: Builder
defaultColorErrorDarkBg = colorVividRedBold

-- | Default color for 'colorNum' for dark backgrounds.  This is
-- 'colorVividGreenBold'.
defaultColorNumDarkBg :: Builder
defaultColorNumDarkBg = colorVividGreenBold

-- | Default colors for 'colorRainbowParens' for dark backgrounds.
defaultColorRainbowParensDarkBg :: [Builder]
defaultColorRainbowParensDarkBg =
  [ colorVividMagentaBold
  , colorVividCyanBold
  , colorVividYellowBold
  , colorDullMagenta
  , colorDullCyan
  , colorDullYellow
  , colorDullMagentaBold
  , colorDullCyanBold
  , colorDullYellowBold
  , colorVividMagenta
  , colorVividCyan
  , colorVividYellow
  ]

-------------------------------------
-- Light background default colors --
-------------------------------------

-- | Default color options for use on a light background.
--
-- 'colorQuote' is 'defaultColorQuoteLightBg'. 'colorString' is
-- 'defaultColorStringLightBg'.  'colorError' is 'defaultColorErrorLightBg'.
-- 'colorNum' is 'defaultColorNumLightBg'.  'colorRainbowParens' is
-- 'defaultColorRainboxParensLightBg'.
defaultColorOptionsLightBg :: ColorOptions
defaultColorOptionsLightBg =
  ColorOptions
  { colorQuote = defaultColorQuoteLightBg
  , colorString = defaultColorStringLightBg
  , colorError = defaultColorErrorLightBg
  , colorNum = defaultColorNumLightBg
  , colorRainbowParens = defaultColorRainbowParensLightBg
  }

-- | Default color for 'colorQuote' for light backgrounds. This is
-- 'colorVividWhiteBold'.
defaultColorQuoteLightBg :: Builder
defaultColorQuoteLightBg = colorVividBlackBold

-- | Default color for 'colorString' for light backgrounds. This is
-- 'colorVividBlueBold'.
defaultColorStringLightBg :: Builder
defaultColorStringLightBg = colorVividBlueBold

-- | Default color for 'colorError' for light backgrounds.  This is
-- 'colorVividRedBold'.
defaultColorErrorLightBg :: Builder
defaultColorErrorLightBg = colorVividRedBold

-- | Default color for 'colorNum' for light backgrounds.  This is
-- 'colorVividGreenBold'.
defaultColorNumLightBg :: Builder
defaultColorNumLightBg = colorVividGreenBold

-- | Default colors for 'colorRainbowParens' for light backgrounds.
defaultColorRainbowParensLightBg :: [Builder]
defaultColorRainbowParensLightBg =
  [ colorVividMagentaBold
  , colorVividCyanBold
  , colorDullMagenta
  , colorDullCyan
  , colorDullMagentaBold
  , colorDullCyanBold
  , colorVividMagenta
  , colorVividCyan
  ]

-----------------------
-- Vivid Bold Colors --
-----------------------

colorVividBlackBold :: Builder
colorVividBlackBold = colorBold `mappend` colorVividBlack

colorVividBlueBold :: Builder
colorVividBlueBold = colorBold `mappend` colorVividBlue

colorVividCyanBold :: Builder
colorVividCyanBold = colorBold `mappend` colorVividCyan

colorVividGreenBold :: Builder
colorVividGreenBold = colorBold `mappend` colorVividGreen

colorVividMagentaBold :: Builder
colorVividMagentaBold = colorBold `mappend` colorVividMagenta

colorVividRedBold :: Builder
colorVividRedBold = colorBold `mappend` colorVividRed

colorVividWhiteBold :: Builder
colorVividWhiteBold = colorBold `mappend` colorVividWhite

colorVividYellowBold :: Builder
colorVividYellowBold = colorBold `mappend` colorVividYellow

-----------------------
-- Dull Bold Colors --
-----------------------

colorDullBlackBold :: Builder
colorDullBlackBold = colorBold `mappend` colorDullBlack

colorDullBlueBold :: Builder
colorDullBlueBold = colorBold `mappend` colorDullBlue

colorDullCyanBold :: Builder
colorDullCyanBold = colorBold `mappend` colorDullCyan

colorDullGreenBold :: Builder
colorDullGreenBold = colorBold `mappend` colorDullGreen

colorDullMagentaBold :: Builder
colorDullMagentaBold = colorBold `mappend` colorDullMagenta

colorDullRedBold :: Builder
colorDullRedBold = colorBold `mappend` colorDullRed

colorDullWhiteBold :: Builder
colorDullWhiteBold = colorBold `mappend` colorDullWhite

colorDullYellowBold :: Builder
colorDullYellowBold = colorBold `mappend` colorDullYellow

------------------
-- Vivid Colors --
------------------

colorVividBlack :: Builder
colorVividBlack = colorHelper Vivid Black

colorVividBlue :: Builder
colorVividBlue = colorHelper Vivid Blue

colorVividCyan :: Builder
colorVividCyan = colorHelper Vivid Cyan

colorVividGreen :: Builder
colorVividGreen = colorHelper Vivid Green

colorVividMagenta :: Builder
colorVividMagenta = colorHelper Vivid Magenta

colorVividRed :: Builder
colorVividRed = colorHelper Vivid Red

colorVividWhite :: Builder
colorVividWhite = colorHelper Vivid White

colorVividYellow :: Builder
colorVividYellow = colorHelper Vivid Yellow

------------------
-- Dull Colors --
------------------

colorDullBlack :: Builder
colorDullBlack = colorHelper Dull Black

colorDullBlue :: Builder
colorDullBlue = colorHelper Dull Blue

colorDullCyan :: Builder
colorDullCyan = colorHelper Dull Cyan

colorDullGreen :: Builder
colorDullGreen = colorHelper Dull Green

colorDullMagenta :: Builder
colorDullMagenta = colorHelper Dull Magenta

colorDullRed :: Builder
colorDullRed = colorHelper Dull Red

colorDullWhite :: Builder
colorDullWhite = colorHelper Dull White

colorDullYellow :: Builder
colorDullYellow = colorHelper Dull Yellow

--------------------
-- Special Colors --
--------------------

-- | Change the intensity to 'BoldIntensity'.
colorBold :: Builder
colorBold = setSGRCodeBuilder [SetConsoleIntensity BoldIntensity]

-- | 'Reset' the console color back to normal.
colorReset :: Builder
colorReset = setSGRCodeBuilder [Reset]

-- | Empty string.
colorNull :: Builder
colorNull = ""

-------------
-- Helpers --
-------------

-- | Helper for creating a 'Builder' for an ANSI escape sequence color based on
-- a 'ColorIntensity' and a 'Color'.
colorHelper :: ColorIntensity -> Color -> Builder
colorHelper colorIntensity color =
  setSGRCodeBuilder [SetColor Foreground colorIntensity color]

-- | Convert a list of 'SGR' to a 'Builder'.
setSGRCodeBuilder :: [SGR] -> Builder
setSGRCodeBuilder = fromString . setSGRCode

