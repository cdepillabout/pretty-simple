{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prettyprinter.Render.Terminal
  (AnsiStyle, Intensity(Dull,Vivid), Color(..))
import qualified Prettyprinter.Render.Terminal as Ansi

-- | These options are for colorizing the output of functions like 'pPrint'.
--
-- For example, if you set 'colorQuote' to something like 'colorBold Vivid Blue',
-- then the quote character (@\"@) will be output as bright blue in bold.
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
  , styleIntensity = Dull
  }

data Style = Style
  { styleColor :: Maybe Color
  , styleBold :: Bool
  , styleIntensity :: Intensity
  }
  deriving (Eq, Show)

color' :: Bool -> Intensity -> Color -> Style
color' b i c = Style
  { styleColor = Just c
  , styleBold = b
  , styleIntensity = i
  }

color :: Intensity -> Color -> Style
color = color' False

colorBold :: Intensity -> Color -> Style
colorBold = color' True

convertStyle :: Style -> AnsiStyle
convertStyle Style {..} =
  (if styleBold then Ansi.bold else mempty)
    <> maybe mempty (col styleIntensity) styleColor
  where
    col = \case
      Vivid -> Ansi.color
      Dull -> Ansi.colorDull
