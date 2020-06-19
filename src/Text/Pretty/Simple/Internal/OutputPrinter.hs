{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pretty.Simple.Internal.OutputPrinter
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple.Internal.OutputPrinter
  where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(ask, reader), runReader)
import Data.Char (isPrint, isSpace, ord)
import Numeric (showHex)
import Data.Foldable (fold)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Typeable (Typeable)
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import System.IO (Handle, hIsTerminalDevice)

import Text.Pretty.Simple.Internal.Color
       (ColorOptions(..), colorReset, defaultColorOptionsDarkBg,
        defaultColorOptionsLightBg)
import Text.Pretty.Simple.Internal.Output
       (NestLevel(..), Output(..), OutputType(..))

-- | Determines whether pretty-simple should check if the output 'Handle' is a
-- TTY device.  Normally, users only want to print in color if the output
-- 'Handle' is a TTY device.
data CheckColorTty
  = CheckColorTty
  -- ^ Check if the output 'Handle' is a TTY device.  If the output 'Handle' is
  -- a TTY device, determine whether to print in color based on
  -- 'outputOptionsColorOptions'. If not, then set 'outputOptionsColorOptions'
  -- to 'Nothing' so the output does not get colorized.
  | NoCheckColorTty
  -- ^ Don't check if the output 'Handle' is a TTY device.  Determine whether to
  -- colorize the output based solely on the value of
  -- 'outputOptionsColorOptions'.
  deriving (Eq, Generic, Show, Typeable)

-- | Data-type wrapping up all the options available when rendering the list
-- of 'Output's.
data OutputOptions = OutputOptions
  { outputOptionsIndentAmount :: Int
  -- ^ Number of spaces to use when indenting.  It should probably be either 2
  -- or 4.
  , outputOptionsColorOptions :: Maybe ColorOptions
  -- ^ If this is 'Nothing', then don't colorize the output.  If this is
  -- @'Just' colorOptions@, then use @colorOptions@ to colorize the output.
  --
  , outputOptionsEscapeNonPrintable :: Bool
  -- ^ Whether to replace non-printable characters with hexadecimal escape
  -- sequences.
  , outputOptionsPrintStringLitsLiterally :: Bool
  -- ^ Output string literals by printing the source characters exactly.
  --
  -- For examples: without this option the printer will insert a newline in
  -- place of @"\n"@, with this options the printer will output @'\'@ and
  -- @'n'@. Similarly the exact escape codes used in the input string will be
  -- replicated, so @"\65"@ will be printed as @"\65"@ and not @"A"@.
  --
  -- When this is enabled the value of 'outputOptionsEscapeNonPrintable' is
  -- unused.
  } deriving (Eq, Generic, Show, Typeable)

-- | Default values for 'OutputOptions' when printing to a console with a dark
-- background.  'outputOptionsIndentAmount' is 4, and
-- 'outputOptionsColorOptions' is 'defaultColorOptionsDarkBg'.
defaultOutputOptionsDarkBg :: OutputOptions
defaultOutputOptionsDarkBg =
  OutputOptions
  { outputOptionsIndentAmount = 4
  , outputOptionsColorOptions = Just defaultColorOptionsDarkBg
  , outputOptionsEscapeNonPrintable = True
  , outputOptionsPrintStringLitsLiterally = False
  }

-- | Default values for 'OutputOptions' when printing to a console with a light
-- background.  'outputOptionsIndentAmount' is 4, and
-- 'outputOptionsColorOptions' is 'defaultColorOptionsLightBg'.
defaultOutputOptionsLightBg :: OutputOptions
defaultOutputOptionsLightBg =
  OutputOptions
  { outputOptionsIndentAmount = 4
  , outputOptionsColorOptions = Just defaultColorOptionsLightBg
  , outputOptionsEscapeNonPrintable = True
  , outputOptionsPrintStringLitsLiterally = False
  }

-- | Default values for 'OutputOptions' when printing using using ANSI escape
-- sequences for color.  'outputOptionsIndentAmount' is 4, and
-- 'outputOptionsColorOptions' is 'Nothing'.
defaultOutputOptionsNoColor :: OutputOptions
defaultOutputOptionsNoColor =
  OutputOptions
  { outputOptionsIndentAmount = 4
  , outputOptionsColorOptions = Nothing
  , outputOptionsEscapeNonPrintable = True
  , outputOptionsPrintStringLitsLiterally = False
  }

-- | Given 'OutputOptions', disable colorful output if the given handle
-- is not connected to a TTY.
hCheckTTY :: MonadIO m => Handle -> OutputOptions -> m OutputOptions
hCheckTTY h options = liftIO $ conv <$> tty
  where
    conv :: Bool -> OutputOptions
    conv True = options
    conv False = options { outputOptionsColorOptions = Nothing }

    tty :: IO Bool
    tty = hIsTerminalDevice h

-- | Given 'OutputOptions' and a list of 'Output', turn the 'Output' into a
-- lazy 'Text'.
render :: OutputOptions -> [Output] -> Text
render options = toLazyText . foldr foldFunc "" . modificationsOutputList
  where
    foldFunc :: Output -> Builder -> Builder
    foldFunc output accum = runReader (renderOutput output) options `mappend` accum

-- | Render a single 'Output' as a 'Builder', using the options specified in
-- the 'OutputOptions'.
renderOutput :: MonadReader OutputOptions m => Output -> m Builder
renderOutput (Output nest OutputCloseBrace) = renderRainbowParenFor nest "}"
renderOutput (Output nest OutputCloseBracket) = renderRainbowParenFor nest "]"
renderOutput (Output nest OutputCloseParen) = renderRainbowParenFor nest ")"
renderOutput (Output nest OutputComma) = renderRainbowParenFor nest ","
renderOutput (Output _ OutputIndent) = do
    indentSpaces <- reader outputOptionsIndentAmount
    pure . mconcat $ replicate indentSpaces " "
renderOutput (Output _ OutputNewLine) = pure "\n"
renderOutput (Output nest OutputOpenBrace) = renderRainbowParenFor nest "{"
renderOutput (Output nest OutputOpenBracket) = renderRainbowParenFor nest "["
renderOutput (Output nest OutputOpenParen) = renderRainbowParenFor nest "("
renderOutput (Output _ (OutputOther string)) = do
  indentSpaces <- reader outputOptionsIndentAmount
  let spaces = replicate (indentSpaces + 2) ' '
  -- TODO: This probably shouldn't be a string to begin with.
  pure $ fromString $ indentSubsequentLinesWith spaces string
renderOutput (Output _ (OutputNumberLit number)) = do
  sequenceFold
    [ useColorNum
    , pure (fromString number)
    , useColorReset
    ]
renderOutput (Output _ (OutputStringLit string)) = do
  options <- ask

  sequenceFold
    [ useColorQuote
    , pure "\""
    , useColorReset
    , useColorString
    -- TODO: This probably shouldn't be a string to begin with.
    , pure (fromString (process options string))
    , useColorReset
    , useColorQuote
    , pure "\""
    , useColorReset
    ]
  where
    process :: OutputOptions -> String -> String
    process opts =
      if outputOptionsPrintStringLitsLiterally opts
        then id
        else if outputOptionsEscapeNonPrintable opts
          then indentSubsequentLinesWith spaces . escapeNonPrintable . readStr
          else indentSubsequentLinesWith spaces . readStr
      where
        spaces :: String
        spaces = replicate (indentSpaces + 2) ' '

        indentSpaces :: Int
        indentSpaces =  outputOptionsIndentAmount opts

        readStr :: String -> String
        readStr s = fromMaybe s . readMaybe $ '"':s ++ "\""
renderOutput (Output _ (OutputCharLit string)) = do
  sequenceFold
    [ useColorQuote
    , pure "'"
    , useColorReset
    , useColorString
    , pure (fromString string)
    , useColorReset
    , useColorQuote
    , pure "'"
    , useColorReset
    ]

-- | Replace non-printable characters with hex escape sequences.
--
-- >>> escapeNonPrintable "\x1\x2"
-- "\\x1\\x2"
--
-- Newlines will not be escaped.
--
-- >>> escapeNonPrintable "hello\nworld"
-- "hello\nworld"
--
-- Printable characters will not be escaped.
--
-- >>> escapeNonPrintable "h\101llo"
-- "hello"
escapeNonPrintable :: String -> String
escapeNonPrintable input = foldr escape "" input

-- Replace an unprintable character except a newline
-- with a hex escape sequence.
escape :: Char -> ShowS
escape c
  | isPrint c || c == '\n' = (c:)
  | otherwise = ('\\':) . ('x':) . showHex (ord c)

-- |
-- >>> indentSubsequentLinesWith "  " "aaa"
-- "aaa"
--
-- >>> indentSubsequentLinesWith "  " "aaa\nbbb\nccc"
-- "aaa\n  bbb\n  ccc"
--
-- >>> indentSubsequentLinesWith "  " ""
-- ""
indentSubsequentLinesWith :: String -> String -> String
indentSubsequentLinesWith indent input =
  intercalate "\n" $ (start ++) $ map (indent ++) $ end
  where (start, end) = splitAt 1 $ lines input

-- | Produce a 'Builder' corresponding to the ANSI escape sequence for the
-- color for the @\"@, based on whether or not 'outputOptionsColorOptions' is
-- 'Just' or 'Nothing', and the value of 'colorQuote'.
useColorQuote :: forall m. MonadReader OutputOptions m => m Builder
useColorQuote = maybe "" colorQuote <$> reader outputOptionsColorOptions

-- | Produce a 'Builder' corresponding to the ANSI escape sequence for the
-- color for the characters of a string, based on whether or not
-- 'outputOptionsColorOptions' is 'Just' or 'Nothing', and the value of
-- 'colorString'.
useColorString :: forall m. MonadReader OutputOptions m => m Builder
useColorString = maybe "" colorString <$> reader outputOptionsColorOptions

useColorError :: forall m. MonadReader OutputOptions m => m Builder
useColorError = maybe "" colorError <$> reader outputOptionsColorOptions

useColorNum :: forall m. MonadReader OutputOptions m => m Builder
useColorNum = maybe "" colorNum <$> reader outputOptionsColorOptions

-- | Produce a 'Builder' corresponding to the ANSI escape sequence for
-- resetting the console color back to the default. Produces an empty 'Builder'
-- if 'outputOptionsColorOptions' is 'Nothing'.
useColorReset :: forall m. MonadReader OutputOptions m => m Builder
useColorReset = maybe "" (const colorReset) <$> reader outputOptionsColorOptions

-- | Produce a 'Builder' representing the ANSI escape sequence for the color of
-- the rainbow parenthesis, given an input 'NestLevel' and 'Builder' to use as
-- the input character.
--
-- If 'outputOptionsColorOptions' is 'Nothing', then just return the input
-- character.  If it is 'Just', then return the input character colorized.
renderRainbowParenFor
  :: MonadReader OutputOptions m
  => NestLevel -> Builder -> m Builder
renderRainbowParenFor nest string =
  sequenceFold [useColorRainbowParens nest, pure string, useColorReset]

useColorRainbowParens
  :: forall m.
     MonadReader OutputOptions m
  => NestLevel -> m Builder
useColorRainbowParens nest = do
  maybeOutputColor <- reader outputOptionsColorOptions
  pure $
    case maybeOutputColor of
      Just ColorOptions {colorRainbowParens} -> do
        let choicesLen = length colorRainbowParens
        if choicesLen == 0
          then ""
          else colorRainbowParens !! (unNestLevel nest `mod` choicesLen)
      Nothing -> ""

-- | This is simply @'fmap' 'fold' '.' 'sequence'@.
sequenceFold :: (Monad f, Monoid a, Traversable t) => t (f a) -> f a
sequenceFold = fmap fold . sequence

-- | A function that performs optimizations and modifications to a list of
-- input 'Output's.
--
-- An sample of an optimization is 'removeStartingNewLine' which just removes a
-- newline if it is the first item in an 'Output' list.
modificationsOutputList :: [Output] -> [Output]
modificationsOutputList =
  removeTrailingSpacesInOtherBeforeNewLine . shrinkWhitespaceInOthers . compressOthers . removeStartingNewLine

-- | Remove a 'OutputNewLine' if it is the first item in the 'Output' list.
--
-- >>> removeStartingNewLine [Output 3 OutputNewLine, Output 3 OutputComma]
-- [Output {outputNestLevel = NestLevel {unNestLevel = 3}, outputOutputType = OutputComma}]
removeStartingNewLine :: [Output] -> [Output]
removeStartingNewLine ((Output _ OutputNewLine) : t) = t
removeStartingNewLine outputs = outputs

-- | Remove trailing spaces from the end of a 'OutputOther' token if it is
-- followed by a 'OutputNewLine', or if it is the final 'Output' in the list.
-- This function assumes that there is a single 'OutputOther' before any
-- 'OutputNewLine' (and before the end of the list), so it must be run after
-- running 'compressOthers'.
--
-- >>> removeTrailingSpacesInOtherBeforeNewLine [Output 2 (OutputOther "foo  "), Output 4 OutputNewLine]
-- [Output {outputNestLevel = NestLevel {unNestLevel = 2}, outputOutputType = OutputOther "foo"},Output {outputNestLevel = NestLevel {unNestLevel = 4}, outputOutputType = OutputNewLine}]
removeTrailingSpacesInOtherBeforeNewLine :: [Output] -> [Output]
removeTrailingSpacesInOtherBeforeNewLine [] = []
removeTrailingSpacesInOtherBeforeNewLine (Output nest (OutputOther string):[]) =
  (Output nest (OutputOther $ dropWhileEnd isSpace string)):[]
removeTrailingSpacesInOtherBeforeNewLine (Output nest (OutputOther string):nl@(Output _ OutputNewLine):t) =
  (Output nest (OutputOther $ dropWhileEnd isSpace string)):nl:removeTrailingSpacesInOtherBeforeNewLine t
removeTrailingSpacesInOtherBeforeNewLine (h:t) = h : removeTrailingSpacesInOtherBeforeNewLine t

-- | If there are two subsequent 'OutputOther' tokens, combine them into just
-- one 'OutputOther'.
--
-- >>> compressOthers [Output 0 (OutputOther "foo"), Output 0 (OutputOther "bar")]
-- [Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputOther "foobar"}]
compressOthers :: [Output] -> [Output]
compressOthers [] = []
compressOthers (Output _ (OutputOther string1):(Output nest (OutputOther string2)):t) =
  compressOthers ((Output nest (OutputOther (string1 `mappend` string2))) : t)
compressOthers (h:t) = h : compressOthers t

-- | In each 'OutputOther' token, compress multiple whitespaces to just one
-- whitespace.
--
-- >>> shrinkWhitespaceInOthers [Output 0 (OutputOther "  hello  ")]
-- [Output {outputNestLevel = NestLevel {unNestLevel = 0}, outputOutputType = OutputOther " hello "}]
shrinkWhitespaceInOthers :: [Output] -> [Output]
shrinkWhitespaceInOthers = fmap shrinkWhitespaceInOther

shrinkWhitespaceInOther :: Output -> Output
shrinkWhitespaceInOther (Output nest (OutputOther string)) =
  Output nest . OutputOther $ shrinkWhitespace string
shrinkWhitespaceInOther other = other

shrinkWhitespace :: String -> String
shrinkWhitespace (' ':' ':t) = shrinkWhitespace (' ':t)
shrinkWhitespace (h:t) = h : shrinkWhitespace t
shrinkWhitespace "" = ""
