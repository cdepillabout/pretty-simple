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

import Data.Data (Data)
import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Text.Pretty.Simple.Internal.Output (Output(..), OutputType(..))

data OutputOptions = OutputOptions
  { indentAmount :: Int
  } deriving (Data, Eq, Generic, Show, Typeable)

renderOutput :: Output -> String
renderOutput (Output nest OutputCloseBrace) = "}"
renderOutput (Output nest OutputCloseBracket) = "]"
renderOutput (Output nest OutputCloseParen) = ")"
renderOutput (Output nest OutputComma) = ","
renderOutput (Output nest OutputIndent) = "    "
renderOutput (Output nest OutputNewLine) = "\n"
renderOutput (Output nest OutputOpenBrace) = "{"
renderOutput (Output nest OutputOpenBracket) = "["
renderOutput (Output nest OutputOpenParen) = "("
renderOutput (Output nest (OutputOther string)) = string
renderOutput (Output nest (OutputStringLit string)) = "\"" <> string <> "\""

renderOutputs :: [Output] -> String
renderOutputs = foldMap renderOutput . modifications

modifications :: [Output] -> [Output]
modifications = shrinkWhitespaceInOthers . compressOthers . removeStartingNewLine

removeStartingNewLine :: [Output] -> [Output]
removeStartingNewLine ((Output _ OutputNewLine) : t) = t
removeStartingNewLine outputs = outputs

compressOthers :: [Output] -> [Output]
compressOthers [] = []
compressOthers (Output _ (OutputOther string1):(Output nest (OutputOther string2)):t) =
  compressOthers ((Output nest (OutputOther (string1 <> string2))) : t)
compressOthers (h:t) = h : compressOthers t

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
