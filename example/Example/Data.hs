{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module      :  Example.Data
Copyright   :  Dennis Gosnell 2017
License     :  BSD3
Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module contains some data types and values that users can use to play
around with pretty-simple.

These data types are also use in the two example programs, as well as the
benchmark for pretty-simple.

Most users should use 'foo' or 'bar'.  'baz' is an extremely large data type,
only used in the benchmark.
-}

module Example.Data where

import Data.Data (Data)
import Data.Typeable (Typeable)

data Foo = Foo
  { foo1 :: Integer
  , foo2 :: [String]
  , foo3 :: Double
  } deriving (Data, Eq, Read, Show, Typeable)

data Bar = Bar
  { bar1 :: Integer
  , bar2 :: [Foo]
  , bar3 :: Double
  } deriving (Data, Eq, Read, Show, Typeable)

data Baz = Baz
  { baz1 :: Bar
  , baz2 :: [Baz]
  } deriving (Data, Eq, Read, Show, Typeable)

foo :: Foo
foo = Foo 3 fooList 3.3

bar :: Bar
bar = Bar 10 (replicate 1 foo) 10.55

bazLevel1 :: Baz
bazLevel1 = Baz bar []

bazLevel2 :: Baz
bazLevel2 = Baz bar $ replicate 50 bazLevel1

baz :: Baz
baz = Baz bar $ replicate 30 bazLevel2

fooList :: [String]
fooList =
  [ "hello"
  , "goodbye"
  , "dog"
  , "cat"
  , "fox"
  , "beaver"
  ]

