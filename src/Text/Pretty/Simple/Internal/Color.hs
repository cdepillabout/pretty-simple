{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Text.Pretty.Simple.Internal.Color
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

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prettyprinter.Render.Terminal
  (AnsiStyle, Intensity(Dull,Vivid), Color(..))
import qualified Prettyprinter.Render.Terminal as Ansi

-- | These options are for colorizing the output of functions like 'pPrint'.
--
-- If you don't want to use a color for one of the options, use 'colorNull'.
data ColorOptions = ColorOptions
  { colorQuote :: Style
  -- ^ Color to use for quote characters (@\"@) around strings.
  , colorString :: Style
  -- ^ Color to use for strings.
  , colorError :: Style
  -- ^ Color for errors, e.g. unmatched brackets.
  , colorNum :: Style
  -- ^ Color to use for numbers.
  , colorRainbowParens :: [Style]
  -- ^ A list of colors to use for rainbow parenthesis output.  Use
  -- '[]' if you don't want rainbow parenthesis.  Use just a single item if you
  -- want all the rainbow parenthesis to be colored the same.
  } deriving (Eq, Generic, Show, Typeable)

-- | Default color options for use on a dark background.
defaultColorOptionsDarkBg :: ColorOptions
defaultColorOptionsDarkBg =
  ColorOptions
  { colorQuote = colorBold Vivid White
  , colorString = colorBold Vivid Blue
  , colorError = colorBold Vivid Red
  , colorNum = colorBold Vivid Green
  , colorRainbowParens =
    [ colorBold Vivid Magenta
    , colorBold Vivid Cyan
    , colorBold Vivid Yellow
    , color Dull Magenta
    , color Dull Cyan
    , color Dull Yellow
    , colorBold Dull Magenta
    , colorBold Dull Cyan
    , colorBold Dull Yellow
    , color Vivid Magenta
    , color Vivid Cyan
    , color Vivid Yellow
    ]
  }

-- | Default color options for use on a light background.
defaultColorOptionsLightBg :: ColorOptions
defaultColorOptionsLightBg =
  ColorOptions
  { colorQuote = colorBold Vivid Black
  , colorString = colorBold Vivid Blue
  , colorError = colorBold Vivid Red
  , colorNum = colorBold Vivid Green
  , colorRainbowParens =
    [ colorBold Vivid Magenta
    , colorBold Vivid Cyan
    , color Dull Magenta
    , color Dull Cyan
    , colorBold Dull Magenta
    , colorBold Dull Cyan
    , color Vivid Magenta
    , color Vivid Cyan
    ]
  }

-- | No styling.
colorNull :: Style
colorNull = Style
  { styleColor = Nothing
  , styleBold = False
  , styleItalic = False
  , styleUnderlined = False
  }

-- | Ways to style terminal output.
data Style = Style
  { styleColor :: Maybe (Color, Intensity)
  , styleBold :: Bool
  , styleItalic :: Bool
  , styleUnderlined :: Bool
  }
  deriving (Eq, Generic, Show, Typeable)

color :: Intensity -> Color -> Style
color i c = colorNull {styleColor = Just (c, i)}

colorBold :: Intensity -> Color -> Style
colorBold i c = (color i c) {styleBold = True}

convertStyle :: Style -> AnsiStyle
convertStyle Style {..} =
  mconcat
    [ maybe mempty (uncurry $ flip col) styleColor
    , if styleBold then Ansi.bold else mempty
    , if styleItalic then Ansi.italicized else mempty
    , if styleUnderlined then Ansi.underlined else mempty
    ]
  where
    col = \case
      Vivid -> Ansi.color
      Dull -> Ansi.colorDull
