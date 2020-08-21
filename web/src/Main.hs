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
import qualified Data.Text as T
import Miso hiding (go)
import Miso.String (ms)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Prettyprinter.Render.Util.SimpleDocTree
import Text.Pretty.Simple
import Text.Pretty.Simple.Internal

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
main =
    runApp $
        startApp
            App
                { initialAction = ()
                , model = ()
                , update = \() () -> noEff ()
                , view = \() -> renderHtml . fmap styleBold . treeForm $ layoutString'
                , events = defaultEvents
                , subs = []
                , logLevel = Off
                , mountPoint = Nothing
                }

layoutString' =
    annotateStyle opts $
        layoutPretty
            defaultLayoutOptions
            $ annotate Open "(" <> annotate Comma "," <> line <> annotate Close ")"

opts :: OutputOptions
opts =
    defaultOutputOptionsNoColor
        { outputOptionsColorOptions =
              Just $
                  ColorOptions
                      { colorQuote = colorNull
                      , colorString = colorNull
                      , colorError = colorNull
                      , colorNum = colorNull
                      , colorRainbowParens = []
                      }
        }

-- TODO having the SDS pre-computed means we don't crash
-- as does const fmapping either of the present styles
-- using defaultOutputOptionsNoColor
-- annotating directly with nullColor

renderHtml :: SimpleDocTree Bool -> View action
renderHtml =
    let go = \case
            STAnn ann content -> map (if ann then id else id) $ go content
            STChar c -> [text $ ms $ T.singleton c]
            STConcat contents -> foldMap go contents
            _ -> []
     in pre_ [] . go
