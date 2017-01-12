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
module Text.Pretty.Simple.Internal.OutputPrinter
  where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Lens (view)
import Control.Lens.TH (makeLenses)
import Control.Monad.Reader (MonadReader, runReader)
import Data.Data (Data)
import Data.Foldable (fold, foldlM)
import Data.Semigroup ((<>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.Console.ANSI
       (Color(..), ColorIntensity(..), ConsoleIntensity(..),
        ConsoleLayer(..), SGR(..), setSGRCode)

import Text.Pretty.Simple.Internal.Output
       (NestLevel(..), Output(..), OutputType(..))

-- | 'UseColor' describes whether or not we want to use color when printing the
-- 'Output' list.
data UseColor
  = NoColor
  | UseColor
  deriving (Data, Eq, Generic, Read, Show, Typeable)

-- | Data-type wrapping up all the options available when rendering the list
-- of 'Output's.
data OutputOptions = OutputOptions
  { _indentAmount :: Int
  -- ^ Number of spaces to use when indenting.  It should probably be either 2
  -- or 4.
  , _useColor :: UseColor
  -- ^ Whether or not to use ansi escape sequences to print colors.
  } deriving (Data, Eq, Generic, Read, Show, Typeable)
makeLenses ''OutputOptions

-- | Default values for 'OutputOptions'.  '_indentAmount' defaults to 4, and
-- '_useColor' defaults to 'UseColor'.
defaultOutputOptions :: OutputOptions
defaultOutputOptions = OutputOptions {_indentAmount = 4, _useColor = UseColor}

render :: OutputOptions -> [Output] -> Text
render options outputs = toLazyText $ runReader (renderOutputs outputs) options

renderOutputs
  :: forall m.
     MonadReader OutputOptions m
  => [Output] -> m Builder
renderOutputs = foldlM foldFunc "" . modificationsOutputList
  where
    foldFunc :: Builder -> Output -> m Builder
    foldFunc accum output = mappend accum <$> renderOutput output

renderRaibowParenFor
  :: MonadReader OutputOptions m
  => NestLevel -> Builder -> m Builder
renderRaibowParenFor nest string =
  sequenceFold [rainbowParen nest, pure string, colorReset]

renderOutput :: MonadReader OutputOptions m => Output -> m Builder
renderOutput (Output nest OutputCloseBrace) = renderRaibowParenFor nest "}"
renderOutput (Output nest OutputCloseBracket) = renderRaibowParenFor nest "]"
renderOutput (Output nest OutputCloseParen) = renderRaibowParenFor nest ")"
renderOutput (Output nest OutputComma) = renderRaibowParenFor nest ","
renderOutput (Output _ OutputIndent) = do
    indentSpaces <- view indentAmount
    pure . mconcat $ replicate indentSpaces " "
renderOutput (Output _ OutputNewLine) = pure "\n"
renderOutput (Output nest OutputOpenBrace) = renderRaibowParenFor nest "{"
renderOutput (Output nest OutputOpenBracket) = renderRaibowParenFor nest "["
renderOutput (Output nest OutputOpenParen) = renderRaibowParenFor nest "("
renderOutput (Output _ (OutputOther string)) =
  -- TODO: This probably shouldn't be a string to begin with.
  pure $ fromString string
renderOutput (Output _ (OutputStringLit string)) = do
  sequenceFold
    [ colorQuote
    , pure "\""
    , colorString
    -- TODO: This probably shouldn't be a string to begin with.
    , pure $ fromString string
    , colorQuote
    , pure "\""
    , colorReset
    ]

sequenceFold :: (Monad f, Monoid a, Traversable t) => t (f a) -> f a
sequenceFold = fmap fold . sequence

-- | A function that performs optimizations and modifications to a list of
-- input 'Output's.
--
-- An sample of an optimization is 'removeStartingNewLine' which just removes a
-- newline if it is the first item in an 'Output' list.
modificationsOutputList :: [Output] -> [Output]
modificationsOutputList = shrinkWhitespaceInOthers . compressOthers . removeStartingNewLine

-- | Remove a 'OutputNewLine' if it is the first item in the 'Output' list.
--
-- >>> removeStartingNewLine [Output 3 OutputNewLine, Output 3 OutputComma]
-- [Output {outputNestLevel = NestLevel {_unNestLevel = 3}, outputOutputType = OutputComma}]
removeStartingNewLine :: [Output] -> [Output]
removeStartingNewLine ((Output _ OutputNewLine) : t) = t
removeStartingNewLine outputs = outputs

-- | If there are two subsequent 'OutputOther' tokens, combine them into just
-- one 'OutputOther'.
--
-- >>> compressOthers [Output 0 (OutputOther "foo"), Output 0 (OutputOther "bar")]
-- [Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputOther "foobar"}]
compressOthers :: [Output] -> [Output]
compressOthers [] = []
compressOthers (Output _ (OutputOther string1):(Output nest (OutputOther string2)):t) =
  compressOthers ((Output nest (OutputOther (string1 <> string2))) : t)
compressOthers (h:t) = h : compressOthers t

-- | In each 'OutputOther' token, compress multiple whitespaces to just one
-- whitespace.
--
-- >>> shrinkWhitespaceInOthers [Output 0 (OutputOther "  hello  ")]
-- [Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputOther " hello "}]
shrinkWhitespaceInOthers :: [Output] -> [Output]
shrinkWhitespaceInOthers = fmap shrinkWhitespaceInOther

shrinkWhitespaceInOther :: Output -> Output
shrinkWhitespaceInOther (Output nest (OutputOther string)) =
  Output nest . OutputOther $ shrinkWhitespace string
shrinkWhitespaceInOther other = other

shrinkWhitespace :: String -> String
shrinkWhitespace (' ':' ':t) = shrinkWhitespace (' ':t)
shrinkWhitespace (h:t) = h : shrinkWhitespace t
shrinkWhitespace "" = ""

-----------------------
-- High-level colors --
-----------------------

colorQuote :: MonadReader OutputOptions m => m Builder
colorQuote = appendColors colorBold colorVividWhite

colorString :: MonadReader OutputOptions m => m Builder
colorString = appendColors colorBold colorVividBlue

colorError :: MonadReader OutputOptions m => m Builder
colorError = appendColors colorBold colorVividRed

colorNum :: MonadReader OutputOptions m => m Builder
colorNum = appendColors colorBold colorVividGreen

rainbowParen
  :: forall m.
     MonadReader OutputOptions m
  => NestLevel -> m Builder
rainbowParen (NestLevel nestLevel) =
  let choicesLen = length rainbowParenChoices
  in rainbowParenChoices !! (nestLevel `mod` choicesLen)
  where
    rainbowParenChoices :: [m Builder]
    rainbowParenChoices =
        [ appendColors colorBold colorVividMagenta
        , appendColors colorBold colorVividCyan
        , appendColors colorBold colorVividYellow
        ]

----------------------
-- Low-level Colors --
----------------------

canUseColor :: MonadReader OutputOptions m => m Bool
canUseColor = do
  color <- view useColor
  case color of
    NoColor -> pure False
    UseColor -> pure True

ifM :: Monad m => m Bool -> a -> a -> m a
ifM comparisonM thenValue elseValue = do
  res <- comparisonM
  case res of
    True -> pure thenValue
    False -> pure elseValue

colorBold :: MonadReader OutputOptions m => m Builder
colorBold = ifM canUseColor (setSGRCodeBuilder [SetConsoleIntensity BoldIntensity]) ""

colorReset :: MonadReader OutputOptions m => m Builder
colorReset = ifM canUseColor (setSGRCodeBuilder [Reset]) ""

colorVividBlue :: MonadReader OutputOptions m => m Builder
colorVividBlue = colorHelper Vivid Blue

colorVividCyan :: MonadReader OutputOptions m => m Builder
colorVividCyan = colorHelper Vivid Cyan

colorVividGreen :: MonadReader OutputOptions m => m Builder
colorVividGreen = colorHelper Vivid Green

colorVividMagenta :: MonadReader OutputOptions m => m Builder
colorVividMagenta = colorHelper Vivid Magenta

colorVividRed :: MonadReader OutputOptions m => m Builder
colorVividRed = colorHelper Vivid Red

colorVividWhite :: MonadReader OutputOptions m => m Builder
colorVividWhite = colorHelper Vivid White

colorVividYellow :: MonadReader OutputOptions m => m Builder
colorVividYellow = colorHelper Vivid Yellow

colorHelper :: MonadReader OutputOptions m => ColorIntensity -> Color -> m Builder
colorHelper colorIntensity color =
  ifM canUseColor (setSGRCodeBuilder [SetColor Foreground colorIntensity color]) ""

appendColors :: MonadReader OutputOptions m => m Builder -> m Builder -> m Builder
appendColors color1 color2 = mappend <$> color1 <*> color2

setSGRCodeBuilder :: [SGR] -> Builder
setSGRCodeBuilder = fromString . setSGRCode
