{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Lens ((<>=), (+=), (-=), use, view)
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Control.Monad.State (MonadState, execState)
import Data.Data (Data)
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import Data.Sequences (fromList, intersperse, singleton)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Text.Pretty.Simple.Internal.Expr (CommaSeparated(..), Expr(..))
import Text.Pretty.Simple.Internal.Output
       (NestLevel(..), Output(..), OutputType(..), unNestLevel)

-- $setup
-- >>> import Control.Monad.State (State)
-- >>> :{
-- let test :: PrinterState -> State PrinterState a -> PrinterState
--     test initState state = execState state initState
--     testInit :: State PrinterState a -> PrinterState
--     testInit = test initPrinterState
-- :}

-- | Newtype around 'Int' to represent a line number.  After a newline, the
-- 'LineNum' will increase by 1.
newtype LineNum = LineNum { unLineNum :: Int }
  deriving (Data, Eq, Generic, Num, Ord, Read, Show, Typeable)
makeLenses ''LineNum

data PrinterState = PrinterState
  { _currLine :: {-# UNPACK #-} !LineNum
  , _nestLevel :: {-# UNPACK #-} !NestLevel
  , _outputList :: !(Seq Output)
  } deriving (Eq, Data, Generic, Show, Typeable)
makeLenses ''PrinterState

-- | Smart-constructor for 'PrinterState'.
printerState :: LineNum -> NestLevel -> Seq Output -> PrinterState
printerState currLineNum nestNum output =
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
  outputList <>= singleton output

addOutputs
  :: MonadState PrinterState m
  => Seq OutputType -> m ()
addOutputs outputTypes = do
  nest <- use nestLevel
  let outputs = Output nest <$> outputTypes
  outputList <>= outputs

initPrinterState :: PrinterState
initPrinterState = printerState 0 (-1) []

-- | Print a surrounding expression (like @\[\]@ or @\{\}@ or @\(\)@).
--
-- If the 'CommaSeparated' expressions are empty, just print the start and end
-- markers.
--
-- >>> testInit $ putSurroundExpr "[" "]" (CommaSeparated [])
-- PrinterState {_currLine = LineNum {unLineNum = 0}, _nestLevel = NestLevel {_unNestLevel = -1}, _outputList = fromList [Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputOpenBracket},Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputCloseBracket}]}
--
-- If there is only one expression, and it will print out on one line, then
-- just print everything all on one line, with spaces around the expressions.
--
-- >>> testInit $ putSurroundExpr "{" "}" (CommaSeparated [[Other "hello"]])
-- PrinterState {_currLine = LineNum {unLineNum = 0}, _nestLevel = NestLevel {_unNestLevel = -1}, _outputList = fromList [Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputOpenBrace},Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputOther " "},Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputOther "hello"},Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputOther " "},Output {outputNestLevel = NestLevel {_unNestLevel = 0}, outputOutputType = OutputCloseBrace}]}
--
-- If there is only one expression, but it will print out on multiple lines,
-- then go to newline and print out on multiple lines.
--
-- >>> 1 + 1  -- TODO: Example here.
-- 2
--
-- If there are multiple expressions, then first go to a newline.
-- Print out on multiple lines.
--
-- >>> 1 + 1  -- TODO: Example here.
-- 2
putSurroundExpr
  :: MonadState PrinterState m
  => OutputType
  -> OutputType
  -> CommaSeparated [Expr] -- ^ comma separated inner expression.
  -> m ()
putSurroundExpr startOutputType endOutputType (CommaSeparated []) = do
  nestLevel += 1
  addOutputs [startOutputType, endOutputType]
  nestLevel -= 1
putSurroundExpr startOutputType endOutputType (CommaSeparated [exprs]) = do
  nestLevel += 1
  let isExprsMultiLine = howManyLines exprs > 1
  when isExprsMultiLine $ do
      newLineAndDoIndent
  addOutputs [startOutputType, OutputOther " "]
  traverse_ putExpression exprs
  if isExprsMultiLine
    then do
      newLineAndDoIndent
    else addOutput $ OutputOther " "
  addOutput endOutputType
  nestLevel -= 1
putSurroundExpr startOutputType endOutputType commaSeparated = do
  nestLevel += 1
  newLineAndDoIndent
  addOutputs [startOutputType, OutputOther " "]
  putCommaSep commaSeparated
  newLineAndDoIndent
  addOutput endOutputType
  nestLevel -= 1
  addOutput $ OutputOther " "

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

howManyLines :: [Expr] -> LineNum
howManyLines = view currLine . runInitPrinterState

doIndent :: MonadState PrinterState m => m ()
doIndent = do
  nest <- use $ nestLevel . unNestLevel
  addOutputs . fromList $ replicate nest OutputIndent

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
  putSurroundExpr OutputOpenBracket OutputCloseBracket commaSeparated
putExpression (Braces commaSeparated) = do
  putSurroundExpr OutputOpenBrace OutputCloseBrace commaSeparated
putExpression (Parens commaSeparated) = do
  putSurroundExpr OutputOpenParen OutputCloseParen commaSeparated
putExpression (StringLit string) = do
  nest <- use nestLevel
  when (nest < 0) $ nestLevel += 1
  addOutput $ OutputStringLit string
putExpression (Other string) = do
  nest <- use nestLevel
  when (nest < 0) $ nestLevel += 1
  addOutput $ OutputOther string

runPrinterState :: PrinterState -> [Expr] -> PrinterState
runPrinterState initState expressions =
  execState (traverse_ putExpression expressions) initState

runInitPrinterState :: [Expr] -> PrinterState
runInitPrinterState = runPrinterState initPrinterState

expressionsToOutputs :: [Expr] -> Seq Output
expressionsToOutputs =
  view outputList . runInitPrinterState . modificationsExprList

-- | A function that performs optimizations and modifications to a list of
-- input 'Expr's.
--
-- An sample of an optimization is 'removeEmptyInnerCommaSeparatedExprList'
-- which removes empty inner lists in a 'CommaSeparated' value.
modificationsExprList :: [Expr] -> [Expr]
modificationsExprList = removeEmptyInnerCommaSeparatedExprList

removeEmptyInnerCommaSeparatedExprList :: [Expr] -> [Expr]
removeEmptyInnerCommaSeparatedExprList = fmap removeEmptyInnerCommaSeparatedExpr

removeEmptyInnerCommaSeparatedExpr :: Expr -> Expr
removeEmptyInnerCommaSeparatedExpr (Brackets commaSeparated) =
  Brackets $ removeEmptyInnerCommaSeparated commaSeparated
removeEmptyInnerCommaSeparatedExpr (Braces commaSeparated) =
  Braces $ removeEmptyInnerCommaSeparated commaSeparated
removeEmptyInnerCommaSeparatedExpr (Parens commaSeparated) =
  Parens $ removeEmptyInnerCommaSeparated commaSeparated
removeEmptyInnerCommaSeparatedExpr other = other

removeEmptyInnerCommaSeparated :: CommaSeparated [Expr] -> CommaSeparated [Expr]
removeEmptyInnerCommaSeparated (CommaSeparated commaSeps) =
  CommaSeparated . fmap removeEmptyInnerCommaSeparatedExprList $
  removeEmptyList commaSeps

-- | Remove empty lists from a list of lists.
--
-- >>> removeEmptyList [[1,2,3], [], [4,5]]
-- [[1,2,3],[4,5]]
--
-- >>> removeEmptyList [[]]
-- []
--
-- >>> removeEmptyList [[1]]
-- [[1]]
--
-- >>> removeEmptyList [[1,2], [10,20], [100,200]]
-- [[1,2],[10,20],[100,200]]
removeEmptyList :: forall a . [[a]] -> [[a]]
removeEmptyList = foldl f []
  where
    f :: [[a]] -> [a] -> [[a]]
    f accum [] = accum
    f accum a = accum <> [a]
