{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Text.Pretty.Simple.Internal.Expr
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple.Internal.Expr
  where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Text.Pretty.Simple.Internal.Color

newtype CommaSeparated a = CommaSeparated { unCommaSeparated :: [a] }
  deriving (Data, Eq, Generic, Show, Typeable)

data Expr
  = Brackets !(CommaSeparated [Expr])
  | Braces !(CommaSeparated [Expr])
  | Parens !(CommaSeparated [Expr])
  | StringLit !String
  | CharLit !String
  | NumberLit !String
  -- ^ We could store this as a 'Rational', say, instead of a 'String'.
  -- However, we will never need to use its value for anything. Indeed, the
  -- only thing we will be doing with it is turning it /back/ into a string
  -- at some stage, so we might as well cut out the middle man and store it
  -- directly like this.
  | CustomExpr Style !String
  | Other !String
  deriving (Eq, Generic, Show, Typeable)
