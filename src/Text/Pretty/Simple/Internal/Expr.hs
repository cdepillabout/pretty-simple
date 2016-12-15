{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pretty.Simple.Expr
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

newtype CommaSeparated a = CommaSeparated { unCommaSeparated :: [a] }
  deriving (Data, Eq, Generic, Show, Typeable)

data Expr
  = Brackets !(CommaSeparated [Expr])
  | Braces !(CommaSeparated [Expr])
  | Parens !(CommaSeparated [Expr])
  | StringLit !String
  | Other !String
  deriving (Data, Eq, Generic, Show, Typeable)
