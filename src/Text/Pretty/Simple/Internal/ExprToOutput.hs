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

import Control.Monad (when)
import Control.Monad.State (MonadState, evalState, gets, modify)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.List (intersperse)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Text.Pretty.Simple.Internal.Expr (CommaSeparated(..), Expr(..))
import Text.Pretty.Simple.Internal.Output
       (NestLevel(..), Output(..), OutputType(..), unNestLevel)
-- $setup
-- >>> import Control.Monad.State (State)
-- >>> :{
-- let test :: PrinterState -> State PrinterState [Output] -> [Output]
--     test initState state = evalState state initState
--     testInit :: State PrinterState [Output] -> [Output]
--     testInit = test initPrinterState
-- :}

-- | Newtype around 'Int' to represent a line number.  After a newline, the
-- 'LineNum' will increase by 1.
newtype LineNum = LineNum { unLineNum :: Int }
  deriving (Data, Eq, Generic, Num, Ord, Read, Show, Typeable)

data PrinterState = PrinterState
  { currLine :: {-# UNPACK #-} !LineNum
  , nestLevel :: {-# UNPACK #-} !NestLevel
  } deriving (Eq, Data, Generic, Show, Typeable)

-- | Smart-constructor for 'PrinterState'.
printerState :: LineNum -> NestLevel -> PrinterState
printerState currLineNum nestNum =
  PrinterState
  { currLine = currLineNum
  , nestLevel = nestNum
  }


addOutput
  :: MonadState PrinterState m
  => OutputType -> m Output
addOutput outputType = do
  nest <- gets nestLevel
  return $ Output nest outputType

addOutputs
  :: MonadState PrinterState m
  => [OutputType] -> m [Output]
addOutputs outputTypes = do
  nest <- gets nestLevel
  return $ Output nest <$> outputTypes

initPrinterState :: PrinterState
initPrinterState = printerState 0 (-1)

-- | Print a surrounding expression (like @\[\]@ or @\{\}@ or @\(\)@).
--
-- If the 'CommaSeparated' expressions are empty, just print the start and end
-- markers.
--
-- >>> testInit $ putSurroundExpr "[" "]" (CommaSeparated [])
-- [Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputOpenBracket},Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputCloseBracket}]
--
-- If there is only one expression, and it will print out on one line, then
-- just print everything all on one line, with spaces around the expressions.
--
-- >>> testInit $ putSurroundExpr "{" "}" (CommaSeparated [[Other "hello"]])
-- [Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputOpenBrace},Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputOther " "},Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputOther "hello"},Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputOther " "},Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputCloseBrace}]
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
  -> m [Output]
putSurroundExpr startOutputType endOutputType (CommaSeparated []) = do
  addToNestLevel 1
  outputs <- addOutputs [startOutputType, endOutputType]
  addToNestLevel (-1)
  return outputs
putSurroundExpr startOutputType endOutputType (CommaSeparated [exprs]) = do
  addToNestLevel 1
  let (thisLayerMulti, nextLayerMulti) = thisAndNextMulti exprs

  maybeNL <- if thisLayerMulti
               then newLineAndDoIndent
               else return []
  start <- addOutputs [startOutputType, OutputOther " "]
  middle <- concat <$> traverse putExpression exprs
  nlOrSpace <- if nextLayerMulti
                 then newLineAndDoIndent
                 else (:[]) <$> (addOutput $ OutputOther " ")
  end <- addOutput endOutputType

  addToNestLevel (-1)

  return $ maybeNL <> start <> middle <> nlOrSpace <> [end]
  where
    thisAndNextMulti = (\(a,b) -> (or a, or b)) . unzip . map isMultiLine

    isMultiLine (Brackets commaSeparated) = isMultiLine' commaSeparated
    isMultiLine (Braces commaSeparated) = isMultiLine' commaSeparated
    isMultiLine (Parens commaSeparated) = isMultiLine' commaSeparated
    isMultiLine _ = (False, False)
    
    isMultiLine' (CommaSeparated []) = (False, False)
    isMultiLine' (CommaSeparated [es]) = (True, fst $ thisAndNextMulti es)
    isMultiLine' _ = (True, True)
putSurroundExpr startOutputType endOutputType commaSeparated = do
  addToNestLevel 1
  nl <- newLineAndDoIndent
  start <- addOutputs [startOutputType, OutputOther " "]
  middle <- putCommaSep commaSeparated
  nl2 <- newLineAndDoIndent
  end <- addOutput endOutputType
  addToNestLevel (-1)
  endSpace <- addOutput $ OutputOther " "

  return $ nl <> start <> middle <> nl2 <> [end, endSpace]


putCommaSep
  :: forall m.
     MonadState PrinterState m
  => CommaSeparated [Expr] -> m [Output]
putCommaSep (CommaSeparated expressionsList) =
  concat <$> (sequence $ intersperse putComma evaledExpressionList)
  where
    evaledExpressionList :: [m [Output]]
    evaledExpressionList =
      (concat <.> traverse putExpression) <$> expressionsList

    (f <.> g) x = f <$> g x

putComma
  :: MonadState PrinterState m
  => m [Output]
putComma = do
  nl <- newLineAndDoIndent
  outputs <- addOutputs [OutputComma, OutputOther " "]
  return $ nl <> outputs

doIndent :: MonadState PrinterState m => m [Output]
doIndent = do
  nest <- gets $ unNestLevel . nestLevel
  addOutputs $ replicate nest OutputIndent

newLine
  :: MonadState PrinterState m
  => m Output
newLine = do
  output <- addOutput OutputNewLine
  addToCurrentLine 1
  return output

newLineAndDoIndent
  :: MonadState PrinterState m
  => m [Output]
newLineAndDoIndent = do
  nl <- newLine
  indent <- doIndent
  return $ nl:indent

addToNestLevel
  :: MonadState PrinterState m
  => NestLevel -> m ()
addToNestLevel diff =
  modify (\printState -> printState {nestLevel = nestLevel printState + diff})

addToCurrentLine
  :: MonadState PrinterState m
  => LineNum -> m ()
addToCurrentLine diff =
  modify (\printState -> printState {currLine = currLine printState + diff})

putExpression :: MonadState PrinterState m => Expr -> m [Output]
putExpression (Brackets commaSeparated) = 
  putSurroundExpr OutputOpenBracket OutputCloseBracket commaSeparated
putExpression (Braces commaSeparated) =
  putSurroundExpr OutputOpenBrace OutputCloseBrace commaSeparated
putExpression (Parens commaSeparated) =
  putSurroundExpr OutputOpenParen OutputCloseParen commaSeparated
putExpression (StringLit string) = do
  nest <- gets nestLevel
  when (nest < 0) $ addToNestLevel 1
  addOutputs [OutputStringLit string, OutputOther " "]
putExpression (Other string) = do
  nest <- gets nestLevel
  when (nest < 0) $ addToNestLevel 1
  (:[]) <$> (addOutput $ OutputOther string)

runPrinterState :: PrinterState -> [Expr] -> [Output]
runPrinterState initState expressions =
  concat $ evalState (traverse putExpression expressions) initState

runInitPrinterState :: [Expr] -> [Output]
runInitPrinterState = runPrinterState initPrinterState

expressionsToOutputs :: [Expr] -> [Output]
expressionsToOutputs = runInitPrinterState . modificationsExprList

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
removeEmptyList = foldr f []
  where
    f :: [a] -> [[a]] -> [[a]]
    f [] accum = accum
    f a accum = [a] <> accum
