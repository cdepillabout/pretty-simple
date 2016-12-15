{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pretty.Simple
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple
  where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Applicative ((<|>))
import Control.Lens (Lens', (%=), (<>=), (.=), (+=), lens, use, view)
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, execState)
import Data.Data (Data)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity)
import Data.MonoTraversable (headEx)
import Data.Semigroup ((<>))
import Data.Sequences (intersperse, tailEx)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Parsec
       (Parsec, ParseError, lookAhead, many, noneOf, optionMaybe, parse,
        spaces, try)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Token

type Parser = Parsec String ()

----------------------------
-- Lexer helper functions --
----------------------------

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser haskellDef

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

---------------------------
-- Data type for grammar --
---------------------------

newtype CommaSeparated a = CommaSeparated { unCommaSeparated :: [a] }
  deriving (Data, Eq, Generic, Show, Typeable)

data Expr
  = Brackets !(CommaSeparated [Expr])
  | Braces !(CommaSeparated [Expr])
  | Parens !(CommaSeparated [Expr])
  | StringLit !String
  | Other !String
  deriving (Data, Eq, Generic, Show, Typeable)

------------
-- Parser --
------------

lala :: Parser [Expr]
lala = do
  spaces
  expr

-- | This is definitely hacky.
expr :: Parser [Expr]
expr = do
  res <- expr'
  maybeNext <- optionMaybe (try (lookAhead expr'))
  case res of
    Other "" -> pure []
    _ ->
      case maybeNext of
        Nothing -> pure [res]
        Just (Other "") -> pure [res]
        _ -> do
          next <- expr
          pure $ res : next

expr' :: Parser Expr
expr' =
  recursiveExpr <|> nonRecursiveExpr

bracketsExpr :: Parser Expr
bracketsExpr = Brackets <$> recursiveSurroundingExpr brackets

bracesExpr :: Parser Expr
bracesExpr = Braces <$> recursiveSurroundingExpr braces

parensExpr :: Parser Expr
parensExpr = Parens <$> recursiveSurroundingExpr parens

recursiveSurroundingExpr :: (forall a. Parser a -> Parser a)
                         -> Parser (CommaSeparated [Expr])
recursiveSurroundingExpr surround = do
  CommaSeparated <$> surround (commaSep expr)

recursiveExpr :: Parser Expr
recursiveExpr = do
  bracketsExpr <|> parensExpr <|> bracesExpr

stringLiteralExpr :: Parser Expr
stringLiteralExpr = StringLit <$> stringLiteral

nonRecursiveExpr :: Parser Expr
nonRecursiveExpr = do
  stringLiteralExpr <|> anyOtherText

anyOtherText :: Parser Expr
anyOtherText = Other <$> many (Text.Parsec.noneOf "[](){},\"")

testString1, testString2 :: String
testString1 = "Just [TextInput {textInputClass = Just (Class {unClass = \"class\"}), textInputId = Just (Id {unId = \"id\"}), textInputName = Just (Name {unName = \"name\"}), textInputValue = Just (Value {unValue = \"value\"}), textInputPlaceholder = Just (Placeholder {unPlaceholder = \"placeholder\"})}, TextInput {textInputClass = Just (Class {unClass = \"class\"}), textInputId = Just (Id {unId = \"id\"}), textInputName = Just (Name {unName = \"name\"}), textInputValue = Just (Value {unValue = \"value\"}), textInputPlaceholder = Just (Placeholder {unPlaceholder = \"placeholder\"})}]"
testString2 = "some stuff (hello [\"dia\\x40iahello\", why wh, bye] ) (bye)"

data PrinterState = PrinterState
  { _currCharOnLine :: Int
  , _indentStack :: [Int]
  , _printerString :: String
  }
makeLenses ''PrinterState

initPrinterState :: PrinterState
initPrinterState = PrinterState 0 [0] ""

prettyPrinter :: [Expr] -> String
prettyPrinter expressions =
  view printerString $ execState (traverse putExpression expressions) initPrinterState

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

-- q :: MonadIO m => String -> m ()
-- q inputString =
--   case p inputString of
--       Left parseError -> do
--         putStrLn $ "Got parse error: " <> tshow parseError
--       Right res ->
--         putStrLn . pack $ prettyPrinter res
