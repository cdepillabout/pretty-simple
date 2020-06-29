{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Copyright   :  Dennis Gosnell 2017
License     :  BSD3
Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This is an short example of using 'pString' from "Text.Pretty.Simple" to
pretty-print JSON.
-}

module Main where

import Data.Aeson (encode)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy as LByteString (ByteString, toStrict)
import Data.Text as Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TextIO (putStrLn)
import qualified Data.Text.Lazy as LText (Text)
import qualified Data.Text.Lazy.IO as LTextIO (putStrLn)
import Text.Pretty.Simple (pPrintString)

import Example.Data (Foo, Bar, bar)

$(deriveJSON defaultOptions ''Foo)
$(deriveJSON defaultOptions ''Bar)

main :: IO ()
main = do
  putStrLn "\nThe following normal \"Data.Aeson.encode\" output:\n"
  putLazyByteStringLn $ encode bar
  putStrLn "\ngets turned into this (using \"Text.Pretty.Simple.pString\"):\n"
  pPrintString . lazyByteStringToString $ encode bar

-- | Convert a 'LByteString.ByteString' to a 'Text.Text' by utf8-encoding it.
lazyByteStringToText :: LByteString.ByteString -> Text.Text
lazyByteStringToText = decodeUtf8 . LByteString.toStrict

-- | Convert a 'LByteString.ByteString' to a 'String' by utf8-encoding it.
lazyByteStringToString :: LByteString.ByteString -> String
lazyByteStringToString = unpack . lazyByteStringToText

-- | Print a 'LByteString.ByteString' to the screen.  Similar to 'putStrLn'.
putLazyByteStringLn :: LByteString.ByteString -> IO ()
putLazyByteStringLn = TextIO.putStrLn . lazyByteStringToText

-- | Print a 'LText.Text' to the screen.  Similar to 'putStrLn'.
putLazyTextLn :: LText.Text -> IO ()
putLazyTextLn = LTextIO.putStrLn
