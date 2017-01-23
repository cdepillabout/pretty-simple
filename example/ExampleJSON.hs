{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Aeson (encode)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy as LByteString (ByteString, toStrict)
import Data.Text as Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TextIO (putStrLn)
import qualified Data.Text.Lazy as LText (Text)
import qualified Data.Text.Lazy.IO as LTextIO (putStrLn)
import Text.Pretty.Simple (pString)

import Example.Data (Foo, Bar, bar)

$(deriveJSON defaultOptions ''Foo)
$(deriveJSON defaultOptions ''Bar)

main :: IO ()
main = do
  putStrLn "\nThe following normal \"Data.Aeson.encode\" output:\n"
  putLazyByteStringLn $ encode bar
  putStrLn "\ngets turned into this (using \"Text.Pretty.Simple.pString\"):\n"
  LTextIO.putStrLn . pString . lazyByteStringToString $ encode bar

lazyByteStringToText :: LByteString.ByteString -> Text.Text
lazyByteStringToText = decodeUtf8 . LByteString.toStrict

lazyByteStringToString :: LByteString.ByteString -> String
lazyByteStringToString = unpack . lazyByteStringToText

putLazyByteStringLn :: LByteString.ByteString -> IO ()
putLazyByteStringLn = TextIO.putStrLn . lazyByteStringToText

putLazyTextLn :: LText.Text -> IO ()
putLazyTextLn = LTextIO.putStrLn
