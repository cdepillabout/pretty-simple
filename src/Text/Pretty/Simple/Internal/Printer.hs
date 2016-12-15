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
import Control.Monad (when)
import Control.Monad.State (MonadState, execState)
import Data.Data (Data)
import Data.Foldable (traverse_)
import Data.MonoTraversable (headEx)
import Data.Semigroup ((<>))
import Data.Sequences (intersperse, tailEx)
import Data.Typeable (Typeable)
import Debug.Trace (traceM)
import GHC.Generics (Generic)

import Text.Pretty.Simple.Internal.Expr (CommaSeparated(..), Expr(..))

-- $setup
-- >>> import Control.Monad.State (State)
-- >>> :{
-- let test :: PrinterState -> State PrinterState a -> PrinterState
--     test initState state = execState state initState
--     testInit :: State PrinterState a -> PrinterState
--     testInit = test initPrinterState
-- :}

data PrinterState = PrinterState
  { _currLine :: Int
  , _currCharOnLine :: Int
  , _indentStack :: [Int]
  , _printerString :: String
  } deriving (Eq, Data, Generic, Show, Typeable)
makeLenses ''PrinterState

printerState :: Int -> Int -> [Int] -> String -> PrinterState
printerState currLineNum currCharNum stack string =
  PrinterState
  { _currLine = currLineNum
  , _currCharOnLine = currCharNum
  , _indentStack = stack
  , _printerString = string
  }

initPrinterState :: PrinterState
initPrinterState = printerState 0 0 [0] ""

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
  currCharOnLine += length symbol
  printerString <>= symbol

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

-- | Print a surrounding expression (like @\[\]@ or @\{\}@ or @\(\)@).
--
-- If the 'CommaSeparated' expressions are empty, just print the start and end
-- markers.
--
-- >>> testInit $ putSurroundExpr "[" "]" (CommaSeparated [])
-- PrinterState {_currLine = 0, _currCharOnLine = 2, _indentStack = [0], _printerString = "[]"}
--
-- >>> let state = printerState 1 5 [5,0] "\nhello"
-- >>> test state $ putSurroundExpr "(" ")" (CommaSeparated [[]])
-- PrinterState {_currLine = 1, _currCharOnLine = 7, _indentStack = [5,0], _printerString = "\nhello()"}
--
-- If there is only one expression, then just print it it all on one line, with
-- spaces around the expressions.
--
-- >>> testInit $ putSurroundExpr "{" "}" (CommaSeparated [[Other "hello", Other "bye"]])
-- PrinterState {_currLine = 0, _currCharOnLine = 12, _indentStack = [0], _printerString = "{ hellobye }"}
--
-- If there are multiple expressions, and this is indent level 0, then print
-- out normally and put each expression on a different line with a comma.
-- No indentation happens.
--
-- >>> let comma = [[Other "hello"], [Other "bye"]]
-- >>> testInit $ putSurroundExpr "[" "]" (CommaSeparated comma)
-- PrinterState {_currLine = 2, _currCharOnLine = 1, _indentStack = [0], _printerString = "[ hello\n, bye\n]"}

-- If there are multiple expressions, and this is not the first thing on the
-- line, then first go to a new line, indent, then continue to print out
-- normally like above.
--
-- >>> let comma = [[Other "foo"], [Other "bar"]]
-- >>> let state = printerState 5 [0] "hello"
-- >>> test $ putSurroundExpr "{" "}" (CommaSeparated comma)
-- PrinterState {_currLine = 3, _currCharOnLine = 4, _indentStack = [0], _printerString = "hello\n    [ foo\n    , bar\n    ]"}
putSurroundExpr
  :: MonadState PrinterState m
  => String -- ^ starting character (@\[@ or @\{@ or @\(@)
  -> String -- ^ ending character (@\]@ or @\}@ or @\)@)
  -> CommaSeparated [Expr] -- ^ comma separated inner expression.
  -> m ()
putSurroundExpr startMarker endMarker (CommaSeparated []) =
  putString $ startMarker <> endMarker
putSurroundExpr startMarker endMarker (CommaSeparated [[]]) =
  putString $ startMarker <> endMarker
putSurroundExpr startMarker endMarker (CommaSeparated [exprs]) = do
  putString $ startMarker <> " "
  startingLineNum <- use currLine
  traverse_ putExpression exprs
  endingLineNum <- use currLine
  if endingLineNum > startingLineNum
    then do
      newLineAndDoIndent
      putString endMarker
    else putString $ " " <> endMarker
putSurroundExpr startMarker endMarker commaSeparated = do
  charOnLine <- use currCharOnLine
  when (charOnLine /= 0) $ do
    newLineAndDoIndent
    putString "    "
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
  currLine += 1

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
