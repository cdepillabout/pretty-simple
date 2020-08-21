{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets (defaultConnectionOptions)
#endif

import Control.Monad (join)
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.String
import Lens.Micro
import qualified Lucid as L
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Prettyprinter.Render.Util.SimpleDocTree

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
    Warp.runSettings (Warp.setPort 8000 $ Warp.setTimeout 3600 Warp.defaultSettings) =<<
        JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

main :: IO ()
main = runApp $ do
    doc <- jsg ("document" :: JSString)
    doc ^. js ("body" :: JSString)
        ^. jss
            ("innerHTML" :: JSString)
            (toJSString $ TL.toStrict t1)

t1 :: TL.Text
t1 =
    TL.pack . show . annotateStyle . layoutPretty defaultLayoutOptions $
        annotate Open "(" <> annotate Comma "," <> line <> annotate Close ")"

data Annotation
    = Open
    | Close
    | Comma

annotateStyle ds = evalState (traverse style ds) initialTape
  where
    style = \case
        Open -> modify moveR *> gets tapeHead
        Close -> gets tapeHead <* modify moveL
        Comma -> gets tapeHead
    initialTape =
        Tape
            { tapeLeft = streamRepeat ()
            , tapeHead = ()
            , tapeRight = streamCycle $ pure ()
            }

-- | A bidirectional Turing-machine tape:
-- infinite in both directions, with a head pointing to one element.
data Tape a = Tape
    { -- | the side of the 'Tape' left of 'tapeHead'
      tapeLeft :: Stream a
    , -- | the focused element
      tapeHead :: a
    , -- | the side of the 'Tape' right of 'tapeHead'
      tapeRight :: Stream a
    }
    deriving (Show)

-- | Move the head left
moveL :: Tape a -> Tape a
moveL (Tape (l :.. ls) c rs) = Tape ls l (c :.. rs)

-- | Move the head right
moveR :: Tape a -> Tape a
moveR (Tape ls c (r :.. rs)) = Tape (c :.. ls) r rs

-- | An infinite list
data Stream a = a :.. Stream a deriving (Show)

-- | Analogous to 'repeat'
streamRepeat :: t -> Stream t
streamRepeat x = x :.. streamRepeat x

-- | Analogous to 'cycle'
-- While the inferred signature here is more general,
-- it would diverge on an empty structure
streamCycle :: NonEmpty a -> Stream a
streamCycle xs = foldr (:..) (streamCycle xs) xs
