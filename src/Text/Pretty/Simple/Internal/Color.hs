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

data ColorOptions = ColorOptions
  { colorQuote :: Builder
  , colorString :: Builder
  , colorError :: Builder
  , colorNum :: Builder
  , colorRainbowParens :: [Builder]
  } deriving (Eq, Generic, Show, Typeable)

------------------------------------
-- Dark background default colors --
------------------------------------

defaultColorOptionsDarkBg :: ColorOptions
defaultColorOptionsDarkBg =
  ColorOptions
  { colorQuote = defaultColorQuoteDarkBg
  , colorString = defaultColorStringDarkBg
  , colorError = defaultColorErrorDarkBg
  , colorNum = defaultColorNumDarkBg
  , colorRainbowParens = defaultColorRainbowParensDarkBg
  }

defaultColorQuoteDarkBg :: Builder
defaultColorQuoteDarkBg = colorVividWhiteBold

defaultColorStringDarkBg :: Builder
defaultColorStringDarkBg = colorVividBlueBold

defaultColorErrorDarkBg :: Builder
defaultColorErrorDarkBg = colorVividRedBold

defaultColorNumDarkBg :: Builder
defaultColorNumDarkBg = colorVividGreenBold

defaultColorRainbowParensDarkBg :: [Builder]
defaultColorRainbowParensDarkBg =
  [ colorVividMagentaBold
  , colorVividCyanBold
  , colorVividYellowBold
  ]

-----------------
-- Bold Colors --
-----------------

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

-------------------
-- Normal Colors --
-------------------

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

--------------------
-- Special Colors --
--------------------

colorBold :: Builder
colorBold = setSGRCodeBuilder [SetConsoleIntensity BoldIntensity]

colorReset :: Builder
colorReset = setSGRCodeBuilder [Reset]

-------------
-- Helpers --
-------------

colorHelper :: ColorIntensity -> Color -> Builder
colorHelper colorIntensity color =
  setSGRCodeBuilder [SetColor Foreground colorIntensity color]

setSGRCodeBuilder :: [SGR] -> Builder
setSGRCodeBuilder = fromString . setSGRCode

