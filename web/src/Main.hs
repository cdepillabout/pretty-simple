{-# LANGUAGE CPP #-}

module Main (main) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets (defaultConnectionOptions)
#endif

import Control.Monad.State
import Data.Text.Prettyprint.Doc
import Language.Javascript.JSaddle
import Lens.Micro

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
            (toJSString string)

string :: String
string =
    show . annotateStyle . layoutPretty defaultLayoutOptions $
        annotate Open "(" <> annotate Comma "," <> annotate Close ")"

data Ann
    = Open
    | Close
    | Comma
    deriving Show

annotateStyle :: Traversable t => t Ann -> t ()
annotateStyle ds =
    evalState
        (traverse f ds)
        Tape
            { tapeLeft = repeat ()
            , tapeHead = ()
            , tapeRight = repeat ()
            }
  where
    f = \case
        Open -> modify move *> gets tapeHead
        Close ->  gets tapeHead
        Comma -> gets tapeHead

move :: Tape a -> Tape ()
move (Tape [] _ _) = Tape [] () []
move (Tape (_ : _) _ _) = Tape [] () []

-- | A bidirectional Turing-machine tape:
-- infinite in both directions, with a head pointing to one element.
data Tape a = Tape
    { -- | the side of the 'Tape' left of 'tapeHead'
      tapeLeft :: [a]
    , -- | the focused element
      tapeHead :: a
    , -- | the side of the 'Tape' right of 'tapeHead'
      tapeRight :: [a]
    }
    deriving (Show)
