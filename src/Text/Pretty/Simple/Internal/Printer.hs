{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pretty.Simple.Internal.Printer
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple.Internal.Printer
  where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Lens (Lens', (%=), (<>=), (.=), (+=), lens, use, view)
import Control.Lens.TH (makeLenses)
import Control.Monad.State (MonadState, execState)
import Data.Foldable (traverse_)
import Data.MonoTraversable (headEx)
import Data.Semigroup ((<>))
import Data.Sequences (intersperse, tailEx)

import Text.Pretty.Simple.Internal.Expr (CommaSeparated(..), Expr(..))

data PrinterState = PrinterState
  { _currCharOnLine :: Int
  , _indentStack :: [Int]
  , _printerString :: String
  }
makeLenses ''PrinterState

initPrinterState :: PrinterState
initPrinterState = PrinterState 0 [0] ""

_headEx :: Lens' [Int] Int
_headEx = lens headEx f
  where
    f :: [Int] -> Int -> [Int]
    f [] _ = []
    f (_:t) a = a:t

-- | This assumes that the indent stack is not empty.
latestIndent :: Lens' PrinterState Int
latestIndent = indentStack . _headEx

popIndent :: MonadState PrinterState m => m ()
popIndent = indentStack %= tailEx

setIndentAtCurrChar
  :: MonadState PrinterState m
  => m ()
setIndentAtCurrChar = do
  currChar <- use currCharOnLine
  indents <- use indentStack
  indentStack .= currChar : indents

putOpeningSymbol :: MonadState PrinterState m => String -> m ()
putOpeningSymbol symbol = do
  setIndentAtCurrChar
  let symbolWithSpace = symbol <> " "
  currCharOnLine += length symbolWithSpace
  printerString <>= symbolWithSpace

putClosingSymbol :: MonadState PrinterState m => String -> m ()
putClosingSymbol symbol = do
  popIndent
  let symbolWithSpace = symbol
  currCharOnLine += length symbolWithSpace
  printerString <>= symbolWithSpace

putComma
  :: MonadState PrinterState m
  => m ()
putComma = do
  newLineAndDoIndent
  let symbolWithSpace = ", " :: String
  currCharOnLine += length symbolWithSpace
  printerString <>= symbolWithSpace

putCommaSep
  :: forall m.
     MonadState PrinterState m
  => CommaSeparated [Expr] -> m ()
putCommaSep (CommaSeparated expressionsList) =
  sequence_ $ intersperse putComma evaledExpressionList
  where
    evaledExpressionList :: [m ()]
    evaledExpressionList =
      fmap (traverse_ putExpression) expressionsList

putString :: MonadState PrinterState m => String -> m ()
putString string = do
  currCharOnLine += length string
  printerString <>= string

putSurroundExpr :: MonadState PrinterState m => String -> String -> CommaSeparated [Expr] -> m ()
putSurroundExpr startMarker endMarker (CommaSeparated []) =
  putString $ startMarker <> endMarker
putSurroundExpr startMarker endMarker (CommaSeparated [[]]) =
  putString $ startMarker <> endMarker
putSurroundExpr startMarker endMarker (CommaSeparated [exprs]) = do
  putString $ startMarker <> " "
  traverse_ putExpression exprs
  putString $ " " <> endMarker
putSurroundExpr startMarker endMarker commaSeparated = do
  newLineAndDoIndent
  putString "  "
  putOpeningSymbol startMarker
  putCommaSep commaSeparated
  newLineAndDoIndent
  putClosingSymbol endMarker

doIndent :: MonadState PrinterState m => m ()
doIndent = do
  indent <- use latestIndent
  currCharOnLine .= indent
  printerString <>= replicate indent ' '

newLine
  :: MonadState PrinterState m
  => m ()
newLine = do
  printerString <>= "\n"
  currCharOnLine .= 0

newLineAndDoIndent
  :: MonadState PrinterState m
  => m ()
newLineAndDoIndent = newLine >> doIndent

putExpression :: MonadState PrinterState m => Expr -> m ()
putExpression (Brackets commaSeparated) = putSurroundExpr "[" "]" commaSeparated
putExpression (Braces commaSeparated) = putSurroundExpr "{" "}" commaSeparated
putExpression (Parens commaSeparated) = putSurroundExpr "(" ")" commaSeparated
putExpression (StringLit string) = putString $ "\"" <> string <> "\""
putExpression (Other string) = putString string

expressionPrint :: [Expr] -> String
expressionPrint expressions =
  view printerString $ execState (traverse putExpression expressions) initPrinterState
