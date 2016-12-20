{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Foldable (foldlM)
import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Text.Pretty.Simple.Internal.Output (Output(..), OutputType(..))

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

render :: OutputOptions -> [Output] -> String
render options outputs = runReader (renderOutputs outputs) options

renderOutputs
  :: forall m.
     MonadReader OutputOptions m
  => [Output] -> m String
renderOutputs = foldlM foldFunc "" . modifications
  where
    foldFunc :: String -> Output -> m String
    foldFunc accum output = mappend accum <$> renderOutput output

renderOutput :: MonadReader OutputOptions m => Output -> m String
renderOutput (Output nest OutputCloseBrace) = pure "}"
renderOutput (Output nest OutputCloseBracket) = pure "]"
renderOutput (Output nest OutputCloseParen) = pure ")"
renderOutput (Output nest OutputComma) = pure ","
renderOutput (Output _ OutputIndent) = do
    indentSpaces <- view indentAmount
    pure "    "
renderOutput (Output _ OutputNewLine) = pure "\n"
renderOutput (Output nest OutputOpenBrace) = pure "{"
renderOutput (Output nest OutputOpenBracket) = pure "["
renderOutput (Output nest OutputOpenParen) = pure "("
renderOutput (Output _ (OutputOther string)) = pure string
renderOutput (Output _ (OutputStringLit string)) = pure $ "\"" <> string <> "\""

-- | A function that performs optimizations and modifications to a list of
-- input 'Output's.
--
-- An sample of an optimization is 'removeStartingNewLine' which just removes a
-- newline if it is the first item in an 'Output' list.
modifications :: [Output] -> [Output]
modifications = shrinkWhitespaceInOthers . compressOthers . removeStartingNewLine

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
