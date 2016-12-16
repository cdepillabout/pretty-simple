{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

type NestLevel = Int

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
  deriving (Data, Eq, Generic, Show, Typeable)

data Output = Output
  { outputNestLevel :: NestLevel
  , outputOutputType :: OutputType
  } deriving (Data, Eq, Generic, Show, Typeable)
