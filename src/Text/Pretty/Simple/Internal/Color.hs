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

import Data.Text.Prettyprint.Doc.Render.Terminal
  (AnsiStyle, Color(..), bold, colorDull, color)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | These options are for colorizing the output of functions like 'pPrint'.
--
-- For example, if you set 'colorQuote' to something like 'colorVividBlueBold',
-- then the quote character (@\"@) will be output as bright blue in bold.
--
-- If you don't want to use a color for one of the options, use 'colorNull'.
data ColorOptions = ColorOptions
  { colorQuote :: AnsiStyle
  -- ^ Color to use for quote characters (@\"@) around strings.
  , colorString :: AnsiStyle
  -- ^ Color to use for strings.
  , colorError :: AnsiStyle
  -- ^ Color for errors, e.g. unmatched brackets.
  , colorNum :: AnsiStyle
  -- ^ Color to use for numbers.
  , colorRainbowParens :: [AnsiStyle]
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
defaultColorQuoteDarkBg :: AnsiStyle
defaultColorQuoteDarkBg = colorVividWhiteBold

-- | Default color for 'colorString' for dark backgrounds. This is
-- 'colorVividBlueBold'.
defaultColorStringDarkBg :: AnsiStyle
defaultColorStringDarkBg = colorVividBlueBold

-- | Default color for 'colorError' for dark backgrounds.  This is
-- 'colorVividRedBold'.
defaultColorErrorDarkBg :: AnsiStyle
defaultColorErrorDarkBg = colorVividRedBold

-- | Default color for 'colorNum' for dark backgrounds.  This is
-- 'colorVividGreenBold'.
defaultColorNumDarkBg :: AnsiStyle
defaultColorNumDarkBg = colorVividGreenBold

-- | Default colors for 'colorRainbowParens' for dark backgrounds.
defaultColorRainbowParensDarkBg :: [AnsiStyle]
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
defaultColorQuoteLightBg :: AnsiStyle
defaultColorQuoteLightBg = colorVividBlackBold

-- | Default color for 'colorString' for light backgrounds. This is
-- 'colorVividBlueBold'.
defaultColorStringLightBg :: AnsiStyle
defaultColorStringLightBg = colorVividBlueBold

-- | Default color for 'colorError' for light backgrounds.  This is
-- 'colorVividRedBold'.
defaultColorErrorLightBg :: AnsiStyle
defaultColorErrorLightBg = colorVividRedBold

-- | Default color for 'colorNum' for light backgrounds.  This is
-- 'colorVividGreenBold'.
defaultColorNumLightBg :: AnsiStyle
defaultColorNumLightBg = colorVividGreenBold

-- | Default colors for 'colorRainbowParens' for light backgrounds.
defaultColorRainbowParensLightBg :: [AnsiStyle]
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

colorVividBlackBold :: AnsiStyle
colorVividBlackBold = colorBold `mappend` colorVividBlack

colorVividBlueBold :: AnsiStyle
colorVividBlueBold = colorBold `mappend` colorVividBlue

colorVividCyanBold :: AnsiStyle
colorVividCyanBold = colorBold `mappend` colorVividCyan

colorVividGreenBold :: AnsiStyle
colorVividGreenBold = colorBold `mappend` colorVividGreen

colorVividMagentaBold :: AnsiStyle
colorVividMagentaBold = colorBold `mappend` colorVividMagenta

colorVividRedBold :: AnsiStyle
colorVividRedBold = colorBold `mappend` colorVividRed

colorVividWhiteBold :: AnsiStyle
colorVividWhiteBold = colorBold `mappend` colorVividWhite

colorVividYellowBold :: AnsiStyle
colorVividYellowBold = colorBold `mappend` colorVividYellow

-----------------------
-- Dull Bold Colors --
-----------------------

colorDullBlackBold :: AnsiStyle
colorDullBlackBold = colorBold `mappend` colorDullBlack

colorDullBlueBold :: AnsiStyle
colorDullBlueBold = colorBold `mappend` colorDullBlue

colorDullCyanBold :: AnsiStyle
colorDullCyanBold = colorBold `mappend` colorDullCyan

colorDullGreenBold :: AnsiStyle
colorDullGreenBold = colorBold `mappend` colorDullGreen

colorDullMagentaBold :: AnsiStyle
colorDullMagentaBold = colorBold `mappend` colorDullMagenta

colorDullRedBold :: AnsiStyle
colorDullRedBold = colorBold `mappend` colorDullRed

colorDullWhiteBold :: AnsiStyle
colorDullWhiteBold = colorBold `mappend` colorDullWhite

colorDullYellowBold :: AnsiStyle
colorDullYellowBold = colorBold `mappend` colorDullYellow

------------------
-- Vivid Colors --
------------------

colorVividBlack :: AnsiStyle
colorVividBlack = color Black

colorVividBlue :: AnsiStyle
colorVividBlue = color Blue

colorVividCyan :: AnsiStyle
colorVividCyan = color Cyan

colorVividGreen :: AnsiStyle
colorVividGreen = color Green

colorVividMagenta :: AnsiStyle
colorVividMagenta = color Magenta

colorVividRed :: AnsiStyle
colorVividRed = color Red

colorVividWhite :: AnsiStyle
colorVividWhite = color White

colorVividYellow :: AnsiStyle
colorVividYellow = color Yellow

------------------
-- Dull Colors --
------------------

colorDullBlack :: AnsiStyle
colorDullBlack = colorDull Black

colorDullBlue :: AnsiStyle
colorDullBlue = colorDull Blue

colorDullCyan :: AnsiStyle
colorDullCyan = colorDull Cyan

colorDullGreen :: AnsiStyle
colorDullGreen = colorDull Green

colorDullMagenta :: AnsiStyle
colorDullMagenta = colorDull Magenta

colorDullRed :: AnsiStyle
colorDullRed = colorDull Red

colorDullWhite :: AnsiStyle
colorDullWhite = colorDull White

colorDullYellow :: AnsiStyle
colorDullYellow = colorDull Yellow

--------------------
-- Special Colors --
--------------------

-- | Change the intensity to 'BoldIntensity'.
colorBold :: AnsiStyle
colorBold = bold

-- | Empty string.
colorNull :: AnsiStyle
colorNull = mempty
