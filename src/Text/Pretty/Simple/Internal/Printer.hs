{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Text.Pretty.Simple.Internal.Printer
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple.Internal.Printer
  where

-- We don't need these imports for later GHCs as all required functions
-- are exported from Prelude
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (join)
import Control.Monad.State (MonadState, evalState, modify, gets)
import Data.Char (isPrint, ord)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe)
import Prettyprinter
  (indent, line', PageWidth(AvailablePerLine), layoutPageWidth, nest,
    concatWith, space, Doc, SimpleDocStream, annotate, defaultLayoutOptions,
    enclose, hcat, layoutSmart, line, unAnnotateS, pretty, group,
    removeTrailingWhitespace)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Numeric (showHex)
import System.IO (Handle, hIsTerminalDevice)
import Text.Read (readMaybe)

import Text.Pretty.Simple.Internal.Expr
  (Expr(..), CommaSeparated(CommaSeparated))
import Text.Pretty.Simple.Internal.ExprParser (expressionParse)
import Text.Pretty.Simple.Internal.Color
       (colorNull, Style, ColorOptions(..), defaultColorOptionsDarkBg,
        defaultColorOptionsLightBg)

-- $setup
-- >>> import Text.Pretty.Simple (pPrintString, pPrintStringOpt)

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

-- | Control how escaped and non-printable are output for strings.
--
-- See 'outputOptionsStringStyle' for what the output looks like with each of
-- these options.
data StringOutputStyle
  = Literal
  -- ^ Output string literals by printing the source characters exactly.
  --
  -- For examples: without this option the printer will insert a newline in
  -- place of @"\n"@, with this options the printer will output @'\'@ and
  -- @'n'@. Similarly the exact escape codes used in the input string will be
  -- replicated, so @"\65"@ will be printed as @"\65"@ and not @"A"@.
  | EscapeNonPrintable
  -- ^ Replace non-printable characters with hexadecimal escape sequences.
  | DoNotEscapeNonPrintable
  -- ^ Output non-printable characters without modification.
  deriving (Eq, Generic, Show, Typeable)

-- | Data-type wrapping up all the options available when rendering the list
-- of 'Output's.
data OutputOptions = OutputOptions
  { outputOptionsIndentAmount :: Int
  -- ^ Number of spaces to use when indenting.  It should probably be either 2
  -- or 4.
  , outputOptionsPageWidth :: Int
  -- ^ The maximum number of characters to fit on to one line.
  , outputOptionsCompact :: Bool
  -- ^ Use less vertical (and more horizontal) space.
  , outputOptionsCompactParens :: Bool
  -- ^ Group closing parentheses on to a single line.
  , outputOptionsInitialIndent :: Int
  -- ^ Indent the whole output by this amount.
  , outputOptionsColorOptions :: Maybe ColorOptions
  -- ^ If this is 'Nothing', then don't colorize the output.  If this is
  -- @'Just' colorOptions@, then use @colorOptions@ to colorize the output.
  --
  , outputOptionsStringStyle :: StringOutputStyle
  -- ^ Controls how string literals are output.
  --
  -- By default, the pPrint functions escape non-printable characters, but
  -- print all printable characters:
  --
  -- >>> pPrintString "\"A \\x42 Ä \\xC4 \\x1 \\n\""
  -- "A B Ä Ä \x1
  -- "
  --
  -- Here, you can see that the character @A@ has been printed as-is.  @\x42@
  -- has been printed in the non-escaped version, @B@.  The non-printable
  -- character @\x1@ has been printed as @\x1@.  Newlines will be removed to
  -- make the output easier to read.
  --
  -- This corresponds to the 'StringOutputStyle' called 'EscapeNonPrintable'.
  --
  -- (Note that in the above and following examples, the characters have to be
  -- double-escaped, which makes it somewhat confusing...)
  --
  -- Another output style is 'DoNotEscapeNonPrintable'.  This is similar
  -- to 'EscapeNonPrintable', except that non-printable characters get printed
  -- out literally to the screen.
  --
  -- >>> pPrintStringOpt CheckColorTty defaultOutputOptionsDarkBg{ outputOptionsStringStyle = DoNotEscapeNonPrintable } "\"A \\x42 Ä \\xC4 \\n\""
  -- "A B Ä Ä
  -- "
  --
  -- If you change the above example to contain @\x1@, you can see that it is
  -- output as a literal, non-escaped character.  Newlines are still removed
  -- for readability.
  --
  -- Another output style is 'Literal'.  This just outputs all escape characters.
  --
  -- >>> pPrintStringOpt CheckColorTty defaultOutputOptionsDarkBg{ outputOptionsStringStyle = Literal } "\"A \\x42 Ä \\xC4 \\x1 \\n\""
  -- "A \x42 Ä \xC4 \x1 \n"
  --
  -- You can see that all the escape characters get output literally, including
  -- newline.
  } deriving (Eq, Generic, Show, Typeable)

-- | Default values for 'OutputOptions' when printing to a console with a dark
-- background.  'outputOptionsIndentAmount' is 4, and
-- 'outputOptionsColorOptions' is 'defaultColorOptionsDarkBg'.
defaultOutputOptionsDarkBg :: OutputOptions
defaultOutputOptionsDarkBg =
  defaultOutputOptionsNoColor
  { outputOptionsColorOptions = Just defaultColorOptionsDarkBg }

-- | Default values for 'OutputOptions' when printing to a console with a light
-- background.  'outputOptionsIndentAmount' is 4, and
-- 'outputOptionsColorOptions' is 'defaultColorOptionsLightBg'.
defaultOutputOptionsLightBg :: OutputOptions
defaultOutputOptionsLightBg =
  defaultOutputOptionsNoColor
  { outputOptionsColorOptions = Just defaultColorOptionsLightBg }

-- | Default values for 'OutputOptions' when printing using using ANSI escape
-- sequences for color.  'outputOptionsIndentAmount' is 4, and
-- 'outputOptionsColorOptions' is 'Nothing'.
defaultOutputOptionsNoColor :: OutputOptions
defaultOutputOptionsNoColor =
  OutputOptions
  { outputOptionsIndentAmount = 4
  , outputOptionsPageWidth = 80
  , outputOptionsCompact = False
  , outputOptionsCompactParens = False
  , outputOptionsInitialIndent = 0
  , outputOptionsColorOptions = Nothing
  , outputOptionsStringStyle = EscapeNonPrintable
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

-- | Parse a string, and generate an intermediate representation,
-- suitable for passing to any /prettyprinter/ backend.
-- Used by 'Simple.pString' etc.
layoutString :: OutputOptions -> String -> SimpleDocStream Style
layoutString opts = annotateStyle opts . layoutStringAbstract opts

layoutStringAbstract :: OutputOptions -> String -> SimpleDocStream Annotation
layoutStringAbstract opts =
    removeTrailingWhitespace
    . layoutSmart defaultLayoutOptions
      {layoutPageWidth = AvailablePerLine (outputOptionsPageWidth opts) 1}
    . indent (outputOptionsInitialIndent opts)
    . prettyExprs' opts
    . expressionParse

-- | Slight adjustment of 'prettyExprs' for the outermost level,
-- to avoid indenting everything.
prettyExprs' :: OutputOptions -> [Expr] -> Doc Annotation
prettyExprs' opts = \case
  [] -> mempty
  x : xs -> prettyExpr opts x <> prettyExprs opts xs

-- | Construct a 'Doc' from multiple 'Expr's.
prettyExprs :: OutputOptions -> [Expr] -> Doc Annotation
prettyExprs opts = hcat . map subExpr
  where
    subExpr x =
      let doc = prettyExpr opts x
      in
        if isSimple x then
          -- keep the expression on the current line
          nest 2 doc
        else
          -- put the expression on a new line, indented (unless grouped)
          nest (outputOptionsIndentAmount opts) $ line' <> doc

-- | Construct a 'Doc' from a single 'Expr'.
prettyExpr :: OutputOptions -> Expr -> Doc Annotation
prettyExpr opts = (if outputOptionsCompact opts then group else id) . \case
  Brackets xss -> list "[" "]" xss
  Braces xss -> list "{" "}" xss
  Parens xss -> list "(" ")" xss
  StringLit s -> join enclose (annotate Quote "\"") $ annotate String $ pretty $ escapeString s
  CharLit s -> join enclose (annotate Quote "'") $ annotate String $ pretty $ escapeString s
  Other s -> pretty s
  NumberLit n -> annotate Num $ pretty n
  where
    escapeString s = case outputOptionsStringStyle opts of
      Literal -> s
      EscapeNonPrintable -> escapeNonPrintable $ readStr s
      DoNotEscapeNonPrintable -> readStr s
    readStr :: String -> String
    readStr s = fromMaybe s . readMaybe $ '"' : s ++ "\""
    list :: Doc Annotation -> Doc Annotation -> CommaSeparated [Expr]
      -> Doc Annotation
    list open close (CommaSeparated xss) =
      enclose (annotate Open open) (annotate Close close) $ case xss of
        [] -> mempty
        [xs] | all isSimple xs ->
          space <> hcat (map (prettyExpr opts) xs) <> space
        _ -> concatWith lineAndCommaSep (map (\xs -> spaceIfNeeded xs <> prettyExprs opts xs) xss)
          <> if outputOptionsCompactParens opts then space else line
          where
            spaceIfNeeded = \case
              Other (' ' : _) : _ -> mempty
              _ -> space
    lineAndCommaSep x y = x <> munless (outputOptionsCompact opts) line' <> annotate Comma "," <> y
    munless b x = if b then mempty else x

-- | Determine whether this expression should be displayed on a single line.
isSimple :: Expr -> Bool
isSimple = \case
  Brackets (CommaSeparated xs) -> isListSimple xs
  Braces (CommaSeparated xs) -> isListSimple xs
  Parens (CommaSeparated xs) -> isListSimple xs
  _ -> True
  where
    isListSimple = \case
      [[e]] -> isSimple e
      _:_ -> False
      [] -> True

-- | Traverse the stream, using a 'Tape' to keep track of the current style.
annotateStyle :: OutputOptions -> SimpleDocStream Annotation
  -> SimpleDocStream Style
annotateStyle opts ds = case outputOptionsColorOptions opts of
  Nothing -> unAnnotateS ds
  Just ColorOptions {..} -> evalState (traverse style ds) initialTape
    where
      style :: MonadState (Tape Style) m => Annotation -> m Style
      style = \case
        Open -> modify moveR *> gets tapeHead
        Close -> gets tapeHead <* modify moveL
        Comma -> gets tapeHead
        Quote -> pure colorQuote
        String -> pure colorString
        Num -> pure colorNum
      initialTape = Tape
        { tapeLeft = streamRepeat colorError
        , tapeHead = colorError
        , tapeRight = streamCycle $ fromMaybe (pure colorNull)
            $ nonEmpty colorRainbowParens
        }

-- | An abstract annotation type, representing the various elements
-- we may want to highlight.
data Annotation
  = Open
  | Close
  | Comma
  | Quote
  | String
  | Num
  deriving (Eq, Show)

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
escapeNonPrintable = foldr escape ""

-- | Replace an unprintable character except a newline
-- with a hex escape sequence.
escape :: Char -> ShowS
escape c
  | isPrint c || c == '\n' = (c:)
  | otherwise = ('\\':) . ('x':) . showHex (ord c)

-- | A bidirectional Turing-machine tape:
-- infinite in both directions, with a head pointing to one element.
data Tape a = Tape
  { tapeLeft  :: Stream a -- ^ the side of the 'Tape' left of 'tapeHead'
  , tapeHead  :: a        -- ^ the focused element
  , tapeRight :: Stream a -- ^ the side of the 'Tape' right of 'tapeHead'
  } deriving Show
-- | Move the head left
moveL :: Tape a -> Tape a
moveL (Tape (l :.. ls) c rs) = Tape ls l (c :.. rs)
-- | Move the head right
moveR :: Tape a -> Tape a
moveR (Tape ls c (r :.. rs)) = Tape (c :.. ls) r rs

-- | An infinite list
data Stream a = a :.. Stream a deriving Show
-- | Analogous to 'repeat'
streamRepeat :: t -> Stream t
streamRepeat x = x :.. streamRepeat x
-- | Analogous to 'cycle'
-- While the inferred signature here is more general,
-- it would diverge on an empty structure
streamCycle :: NonEmpty a -> Stream a
streamCycle xs = foldr (:..) (streamCycle xs) xs
