{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
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
import Data.String (IsString, fromString)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Datatype representing how much something is nested.
--
-- For example, a 'NestLevel' of 0 would mean an 'Output' token
-- is at the very highest level, not in any braces.
--
-- A 'NestLevel' of 1 would mean that an 'Output' token is in one single pair
-- of @\{@ and @\}@, or @\[@ and @\], or @\(@ and @\)@.
--
-- A 'NestLevel' of 2 would mean that an 'Output' token is two levels of
-- brackets, etc.
newtype NestLevel = NestLevel { _unNestLevel :: Int }
  deriving (Data, Eq, Generic, Num, Ord, Read, Show, Typeable)
makeLenses ''NestLevel

-- | These are the output tokens that we will be printing to the screen.
data OutputType
  = OutputCloseBrace
  -- ^ This represents the @\}@ character.
  | OutputCloseBracket
  -- ^ This represents the @\]@ character.
  | OutputCloseParen
  -- ^ This represents the @\)@ character.
  | OutputComma
  -- ^ This represents the @\,@ character.
  | OutputIndent
  -- ^ This represents an indentation.
  | OutputNewLine
  -- ^ This represents the @\\n@ character.
  | OutputOpenBrace
  -- ^ This represents the @\{@ character.
  | OutputOpenBracket
  -- ^ This represents the @\[@ character.
  | OutputOpenParen
  -- ^ This represents the @\(@ character.
  | OutputOther !String
  -- ^ This represents some collection of characters that don\'t fit into any
  -- of the other tokens.
  | OutputStringLit !String
  -- ^ This represents a string literal.  For instance, @\"foobar\"@.
  deriving (Data, Eq, Generic, Read, Show, Typeable)

-- | 'IsString' (and 'fromString') should generally only be used in tests and
-- debugging.  There is no way to represent 'OutputIndent' and
-- 'OutputStringLit'.
instance IsString OutputType where
    fromString :: String -> OutputType
    fromString "}" = OutputCloseBrace
    fromString "]" = OutputCloseBracket
    fromString ")" = OutputCloseParen
    fromString "," = OutputComma
    fromString "\n" = OutputNewLine
    fromString "{" = OutputOpenBrace
    fromString "[" = OutputOpenBracket
    fromString "(" = OutputOpenParen
    fromString string = OutputOther string

-- | An 'OutputType' token together with a 'NestLevel'.  Basically, each
-- 'OutputType' keeps track of its own 'NestLevel'.
data Output = Output
  { outputNestLevel :: {-# UNPACK #-} !NestLevel
  , outputOutputType :: !OutputType
  } deriving (Data, Eq, Generic, Read, Show, Typeable)
