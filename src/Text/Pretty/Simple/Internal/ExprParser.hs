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
parseStringLit (c:cs)   = (c:cs', rest)
  where (cs', rest) = parseStringLit cs

parseOther :: String -> (String, String)
parseOther [] = ("", "")
parseOther s@(c:cs)
  | c `elem` ("{[()]}\"," :: String) = ("", s)
  | otherwise = let (toParse, rest) = parseOther cs
                 in (c : toParse, rest)
