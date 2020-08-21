{-# LANGUAGE CPP #-}

module Main (main) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets (defaultConnectionOptions)
#endif

import Control.Monad.State
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
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
string = show . annotateStyle . layoutPretty defaultLayoutOptions $ annotate False "(" <> annotate True "," <> annotate True ")"

annotateStyle :: Traversable t => t Bool -> t ()
annotateStyle ds = evalState (traverse f ds) $ () :| repeat ()
  where
    f = \case
        False -> modify move *> gets NE.head
        True -> gets NE.head

move :: NonEmpty a -> NonEmpty ()
move = \case
    _ :| _ -> pure ()
