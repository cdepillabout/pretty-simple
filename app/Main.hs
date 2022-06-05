module Main where

-- This is a small executable that will pretty-print anything from stdin.
-- It can be installed to `~/.local/bin` if you enable the flag `buildexe` like so:
--
-- @
--   $ stack install pretty-simple-2.0.1.1 --flag pretty-simple:buildexe
-- @
--
-- When you run it, you can paste something you want formatted on stdin, then
-- press @Ctrl-D@.  It will print the formatted version on stdout:
--
-- @
--   $ pretty-simple
--   [(Just 3, Just 4)]
--
--   ^D
--
--   [
--       ( Just 3
--       , Just 4
--       )
--   ]
-- @

import Data.Monoid ((<>))
import Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Data.Version (showVersion)
import Options.Applicative
       ( Parser, ReadM, execParser, fullDesc, help, helper, info, infoOption
       , long, option, progDesc, readerError, short, showDefaultWith, str
       , switch, value)
import Paths_pretty_simple (version)
import Text.Pretty.Simple
       ( pStringOpt, OutputOptions
       , defaultOutputOptionsDarkBg
       , defaultOutputOptionsLightBg
       , defaultOutputOptionsNoColor
       , outputOptionsCompact
       )

data Color = DarkBg
           | LightBg
           | NoColor

data Args = Args
  { color :: Color
  , compact :: Bool
  }

colorReader :: ReadM Color
colorReader = do
  string <- str
  case string of
    "dark-bg"  -> pure DarkBg
    "light-bg" -> pure LightBg
    "no-color" -> pure NoColor
    x          -> readerError $ "Could not parse " <> x <> " as a color."

args :: Parser Args
args = Args
    <$> option colorReader
        ( long "color"
       <> short 'c'
       <> help "Select printing color. Available options: dark-bg (default), light-bg, no-color."
       <> showDefaultWith (const "dark-bg")
       <> value DarkBg
        )
    <*> switch
        ( long "compact"
       <> short 'C'
       <> help "Compact output"
        )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (showVersion version)
    ( long "version"
   <> short 'V'
   <> help "Show version"
    )

main :: IO ()
main = do
  args' <- execParser opts
  input <- T.getContents
  let output = pStringOpt (getPrintOpt args') $ unpack input
  LT.putStrLn output
  where
    opts = info (helper <*> versionOption <*> args)
      ( fullDesc
     <> progDesc "Format Haskell data types with indentation and highlighting"
      )

    getPrintOpt :: Args -> OutputOptions
    getPrintOpt as =
      (getColorOpt (color as)) {outputOptionsCompact = compact as}

    getColorOpt :: Color -> OutputOptions
    getColorOpt DarkBg  = defaultOutputOptionsDarkBg
    getColorOpt LightBg = defaultOutputOptionsLightBg
    getColorOpt NoColor = defaultOutputOptionsNoColor
