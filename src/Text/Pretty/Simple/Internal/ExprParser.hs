{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pretty.Simple.Internal.ExprParser
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple.Internal.ExprParser
  where

import Text.Pretty.Simple.Internal.Expr (CommaSeparated(..), Expr(..))
import Control.Arrow (first)
import Data.Char (isAlpha, isDigit, isLower)

testString1, testString2 :: String
testString1 = "Just [TextInput {textInputClass = Just (Class {unClass = \"class\"}), textInputId = Just (Id {unId = \"id\"}), textInputName = Just (Name {unName = \"name\"}), textInputValue = Just (Value {unValue = \"value\"}), textInputPlaceholder = Just (Placeholder {unPlaceholder = \"placeholder\"})}, TextInput {textInputClass = Just (Class {unClass = \"class\"}), textInputId = Just (Id {unId = \"id\"}), textInputName = Just (Name {unName = \"name\"}), textInputValue = Just (Value {unValue = \"value\"}), textInputPlaceholder = Just (Placeholder {unPlaceholder = \"placeholder\"})}]"
testString2 = "some stuff (hello [\"dia\\x40iahello\", why wh, bye] ) (bye)"

expressionParse :: String -> [Expr]
expressionParse = fst . parseExprs

parseExpr :: String -> (Expr, String)
parseExpr ('(':rest) = first (Parens . CommaSeparated) $ parseCSep ')' rest
parseExpr ('[':rest) = first (Brackets . CommaSeparated) $ parseCSep ']' rest
parseExpr ('{':rest) = first (Braces . CommaSeparated) $ parseCSep '}' rest
parseExpr ('"':rest) = first StringLit $ parseStringLit rest
parseExpr (c:rest) | isDigit c = first NumberLit $ parseNumberLit c rest
parseExpr other      = first Other $ parseOther other

parseExprs :: String -> ([Expr], String)
parseExprs [] = ([], "")
parseExprs s@(c:_)
  | c `elem` (")]}," :: String) = ([], s)
  | otherwise = let (parsed, rest') = parseExpr s
                    (toParse, rest) = parseExprs rest'
                 in (parsed : toParse, rest)

parseCSep :: Char -> String -> ([[Expr]], String)
parseCSep _ [] = ([], "")
parseCSep end s@(c:cs)
  | c == end = ([], cs)
  -- Mismatch condition; if the end does not match, there is a mistake
  -- Perhaps there should be a Missing constructor for Expr
  | c `elem` (")]}" :: String) = ([], s)
  | c == ',' = parseCSep end cs
  | otherwise = let (parsed, rest') = parseExprs s
                    (toParse, rest) = parseCSep end rest'
                 in (parsed : toParse, rest)

parseStringLit :: String -> (String, String)
parseStringLit [] = ("", "")
parseStringLit ('"':rest) = ("", rest)
parseStringLit ('\\':c:cs) = ('\\':c:cs', rest)
  where (cs', rest) = parseStringLit cs
parseStringLit (c:cs) = (c:cs', rest)
  where (cs', rest) = parseStringLit cs

-- | Parses integers and reals, like @123@ and @45.67@.
--
-- To be more precise, any numbers matching the regex @\\d+(\\.\\d+)?@ should
-- get parsed by this function.
parseNumberLit :: Char -> String -> (String, String)
parseNumberLit c rest1 =
  case rest2 of
    []        -> (c:base, "")
    '.':rest3 -> first ((c :) . (base ++) . ('.' :)) (span isDigit rest3)
    _         -> (c:base, rest2)
  where (base, rest2) = span isDigit rest1

-- | This is almost the same as the function
--
-- > parseOtherSimple = span $ \c ->
-- >   notElem c ("{[()]}\"," :: String) && not (isDigit c)
--
-- The behaviour of @parseOtherSimple@ here is to keep on going consuming input
-- until we hit a special character or a digit.
--
-- The full behaviour of /this/ function can be described as: keep on going
-- consuming input, stopping only when we hit a special character or a digit.
-- However, if we appear to be in the middle of a Haskell-style identifier
-- (e.g. @foo123@) and we hit a digit, then keep going anyway.
parseOther :: String -> (String, String)
parseOther = go False
  where
    go :: Bool {-^ in an identifier? -} -> String -> (String, String)
    go _ [] = ("", "")
    go insideIdent cs@(c:cs')
      | c `elem` ("{[()]}\"," :: String) = ("", cs)
      | isDigit c && not insideIdent = ("", cs)
      | insideIdent = first (c :) (go (isIdentRest c) cs')
      | otherwise = first (c :) (go (isIdentBegin c) cs')

    isIdentBegin :: Char -> Bool
    isIdentBegin '_' = True
    isIdentBegin c = isLower c

    isIdentRest :: Char -> Bool
    isIdentRest '_' = True
    isIdentRest '\'' = True
    isIdentRest c = isAlpha c || isDigit c

-- |
-- Handle escaped characters correctly
--
-- >>> parseExprs $ "Foo \"hello \\\"world!\""
-- ([Other "Foo ",StringLit "hello \\\"world!"],"")
