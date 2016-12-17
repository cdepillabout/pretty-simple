{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Text.Pretty.Simple.Internal.Output
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple.Internal.Output
  where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Lens.TH (makeLenses)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

newtype NestLevel = NestLevel { _unNestLevel :: Int }
  deriving (Data, Eq, Generic, Num, Ord, Read, Show, Typeable)
makeLenses ''NestLevel

data OutputType
  = OutputCloseBrace
  | OutputCloseBracket
  | OutputCloseParen
  | OutputComma
  | OutputIndent
  | OutputNewLine
  | OutputOpenBrace
  | OutputOpenBracket
  | OutputOpenParen
  | OutputOther !String
  | OutputStringLit !String
  deriving (Data, Eq, Generic, Read, Show, Typeable)

data Output = Output
  { outputNestLevel :: NestLevel
  , outputOutputType :: OutputType
  } deriving (Data, Eq, Generic, Read, Show, Typeable)
