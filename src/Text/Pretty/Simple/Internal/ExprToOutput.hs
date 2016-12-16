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
module Text.Pretty.Simple.Internal.ExprToOutput
  where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Lens
       (Lens', (%=), (<>=), (.=), (+=), (-=), lens, use, view)
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Control.Monad.State (MonadState, execState)
import Data.Data (Data)
import Data.Foldable (traverse_)
import Data.MonoTraversable (headEx)
import Data.Semigroup ((<>))
import Data.Sequences (intersperse, tailEx)
import Data.Typeable (Typeable)
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
  , _nestLevel :: Int
  , _outputList :: [Output]
  } deriving (Eq, Data, Generic, Show, Typeable)
makeLenses ''PrinterState

printerState :: Int -> Int -> [Int] -> Int -> [Output] -> PrinterState
printerState currLineNum currCharNum stack nestNum output =
  PrinterState
  { _currLine = currLineNum
  , _nestLevel = nestNum
  , _outputList = output
  }

addOutput
  :: MonadState PrinterState m
  => OutputType -> m ()
addOutput outputType = do
  nest <- use nestLevel
  let output = Output nest outputType
  outputList <>= [output]

addOutputs
  :: MonadState PrinterState m
  => [OutputType] -> m ()
addOutputs outputTypes = do
  nest <- use nestLevel
  let outputs = Output nest <$> outputTypes
  outputList <>= outputs

initPrinterState :: PrinterState
initPrinterState = printerState 0 0 [0] 0 []

-- | Print a surrounding expression (like @\[\]@ or @\{\}@ or @\(\)@).
--
-- If the 'CommaSeparated' expressions are empty, just print the start and end
-- markers.
--
-- >>> testInit $ putSurroundExpr "[" "]" (CommaSeparated [])
-- PrinterState {_currLine = 0, _currCharOnLine = 2, _indentStack = [0], _nestLevel = 0, _outputList = "[]"}
--
-- >>> let state = printerState 1 5 [5,0] 10 "\nhello"
-- >>> test state $ putSurroundExpr "(" ")" (CommaSeparated [[]])
-- PrinterState {_currLine = 1, _currCharOnLine = 7, _indentStack = [5,0], _nestLevel = 10, _outputList = "\nhello()"}
--
-- If there is only one expression, then just print it it all on one line, with
-- spaces around the expressions.
--
-- >>> testInit $ putSurroundExpr "{" "}" (CommaSeparated [[Other "hello", Other "bye"]])
-- PrinterState {_currLine = 0, _currCharOnLine = 12, _indentStack = [0], _nestLevel = 0, _outputList = "{ hellobye }"}
--
-- If there are multiple expressions, and this is indent level 0, then print
-- out normally and put each expression on a different line with a comma.
-- No indentation happens.
--
-- >>> let comma = [[Other "hello"], [Other "bye"]]
-- >>> testInit $ putSurroundExpr "[" "]" (CommaSeparated comma)
-- PrinterState {_currLine = 2, _currCharOnLine = 1, _indentStack = [0], _nestLevel = 0, _outputList = "[ hello\n, bye\n]"}

-- If there are multiple expressions, and this is not the first thing on the
-- line, then first go to a new line, indent, then continue to print out
-- normally like above.
--
-- >>> let comma = [[Other "foo"], [Other "bar"]]
-- >>> let state = printerState 5 [0] 0 "hello"
-- >>> test $ putSurroundExpr "{" "}" (CommaSeparated comma)
-- PrinterState {_currLine = 3, _currCharOnLine = 4, _indentStack = [0], _nestLevel = 0, _outputList = "hello\n    [ foo\n    , bar\n    ]"}
putSurroundExpr
  :: MonadState PrinterState m
  => OutputType
  -> OutputType
  -> CommaSeparated [Expr] -- ^ comma separated inner expression.
  -> m ()
putSurroundExpr startOutputType endOutputType (CommaSeparated []) =
  addOutputs [startOutputType, endOutputType]
putSurroundExpr startOutputType endOutputType (CommaSeparated [exprs]) = do
  let isExprsMultiLine = howManyLines exprs
  when isExprsMultiLine newLineAndDoIndent
  addOutputs [startOutputType, OutputOther " "]
  traverse_ putExpression exprs
  if isExprsMultiLine
    then newLineAndDoIndent
    else addOutput $ OutputOther " "
  addOutputs endOutputType
putSurroundExpr startOutputType endOutputType commaSeparated = do
  newLineAndDoIndent
  addOutputs [startOutputType, OutputOther " "]
  putCommaSep commaSeparated
  newLineAndDoIndent
  addOutputs [endOutputType, OutputOther " "]

putCommaSep
  :: forall m.
     MonadState PrinterState m
  => CommaSeparated [Expr] -> m ()
putCommaSep (CommaSeparated expressionsList) =
  sequence_ $ intersperse putComma evaledExpressionList
  where
    evaledExpressionList :: [m ()]
    evaledExpressionList =
      traverse_ putExpression <$> expressionsList

putComma
  :: MonadState PrinterState m
  => m ()
putComma = do
  newLineAndDoIndent
  addOutputs [OutputComma, OutputOther " "]

howManyLines :: [Expr] -> Int
howManyLines = view currLine . runInitPrinterState

doIndent :: MonadState PrinterState m => m ()
doIndent = do
  nest <- use nestLevel
  addOutputs $ replicate nest OutputIndent

newLine
  :: MonadState PrinterState m
  => m ()
newLine = do
  addOutput OutputNewLine
  currLine += 1

newLineAndDoIndent
  :: MonadState PrinterState m
  => m ()
newLineAndDoIndent = newLine >> doIndent

putExpression :: MonadState PrinterState m => Expr -> m ()
putExpression (Brackets commaSeparated) = do
    nestLevel += 1
    putSurroundExpr OutputOpenBracket OutputCloseBracket commaSeparated
    nestLevel -= 1
putExpression (Braces commaSeparated) = do
    nestLevel += 1
    putSurroundExpr OutputOpenBrace OutputCloseBrace commaSeparated
    nestLevel -= 1
putExpression (Parens commaSeparated) = do
    nestLevel += 1
    putSurroundExpr OutputOpenParen OutputCloseParen commaSeparated
    nestLevel -= 1
putExpression (StringLit string) = addOutput $ OutputStringLit string
putExpression (Other string) = addOutput $ OutputOther string

runPrinterState :: PrinterState -> [Expr] -> PrinterState
runPrinterState initState expressions =
  execState (traverse_ putExpression expressions) initState

runInitPrinterState :: [Expr] -> PrinterState
runInitPrinterState = runPrinterState initPrinterState

expressionsToOutputs :: [Expr] -> [Output]
expressionsToOutputs = view outputList . runInitPrinterState
