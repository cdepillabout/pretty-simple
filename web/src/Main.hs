{-# LANGUAGE CPP #-}

module Main (main) where

import Miso hiding (go, set)

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Wai
import Network.WebSockets (defaultConnectionOptions)
#endif

import Control.Monad.State (evalState, gets, modify)
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro (over, set)
import Miso.String (MisoString, fromMisoString, ms, toLower)
import qualified Miso.String as Miso
import Prettyprinter.Render.Util.SimpleDocTree (SimpleDocTree (..), treeForm)
import Text.Pretty.Simple (OutputOptions, StringOutputStyle (..), defaultOutputOptionsNoColor)
import Text.Pretty.Simple.Internal (Annotation (..), layoutStringAbstract)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
    Warp.runSettings (Warp.setPort 8000 $ Warp.setTimeout 3600 Warp.defaultSettings)
        =<< JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) app
  where
    app req =
        case Wai.pathInfo req of
            ["style.css"] -> Wai.staticApp (Wai.defaultWebAppSettings ".") req
            _ -> JSaddle.jsaddleApp req
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

data Model = Model
    { inputText :: MisoString
    , outputOptions :: OutputOptions
    }
    deriving (Show, Eq, Generic)

data Action
    = NoOp
    | Log MisoString
    | TextEntered MisoString
    | OptsChanged (OutputOptions -> OutputOptions)

main :: IO ()
main = runApp $ startApp App{..}
  where
    initialAction = NoOp
    model =
        Model
            { inputText = ""
            , outputOptions = defaultOutputOptionsNoColor
            }
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing -- mount at `body`
    logLevel = Off

updateModel :: Action -> Model -> Effect Action Model
updateModel = \case
    NoOp -> noEff
    Log t -> (<# (consoleLog t >> pure NoOp))
    TextEntered t -> noEff . set #inputText t
    OptsChanged f -> noEff . over #outputOptions f

viewModel :: Model -> View Action
viewModel m =
    div_
        [class_ "root"]
        [ textArea [class_ "input"] TextEntered ""
        , div_
            [class_ "opts"]
            [ checkBox [] (setOpts #outputOptionsCompact) "Compact"
            , checkBox [] (setOpts #outputOptionsCompactParens) "Compact parentheses"
            , slider [] 10 (setOpts #outputOptionsIndentAmount) "Indentation"
            , slider [] 20 (setOpts #outputOptionsInitialIndent) "Initial indent"
            , slider [] 240 (setOpts #outputOptionsPageWidth) "Page width"
            , div_
                []
                [ text "Non-printable characters"
                , selectMenu
                    []
                    (setOpts #outputOptionsStringStyle)
                    Log
                    [ ("Escape", EscapeNonPrintable)
                    , ("Don't escape", DoNotEscapeNonPrintable)
                    , ("Literal", Literal)
                    ]
                ]
            ]
        , pPrintStringHtml [class_ "output"] (outputOptions m) . fromMisoString $ inputText m
        , link_
            [ rel_ "stylesheet"
            , href_ "style.css"
            ]
        ]
  where
    setOpts l = OptsChanged . set l

data ParensLevel
    = Parens0
    | Parens1
    | Parens2
    deriving (Eq, Show, Bounded, Enum)

-- TODO ideally, we'd reuse `layoutString`, and just map over its result, but `annotateStyle` crashes on GHCJS 8.6:
-- https://github.com/ghcjs/ghcjs/issues/794
pPrintStringHtml :: [Attribute act] -> OutputOptions -> String -> View act
pPrintStringHtml as opts = renderHtml as . treeForm . annotateWithIndentation . layoutStringAbstract opts
  where
    annotateWithIndentation =
        flip evalState (prev Parens0) . traverse \ann ->
            (++ [Class "annotation", toClassName @Annotation ann]) <$> case ann of
                Open -> modify next *> g
                Close -> g <* modify prev
                Comma -> g
                _ -> pure []
      where
        g = gets (pure . toClassName @ParensLevel)
        toClassName :: Show a => a -> Class
        toClassName = Class . toLower . ms . show

{- Wrappers around HTML elements -}

checkBox :: [Attribute action] -> (Bool -> action) -> MisoString -> View action
checkBox as f t =
    label_
        as
        [ text t
        , input_ [type_ "checkbox", onChecked $ f . unChecked]
        ]
  where
    unChecked (Checked b) = b

slider :: [Attribute action] -> Int -> (Int -> action) -> MisoString -> View action
slider as m f t =
    label_
        as
        [ text t
        , input_
            [ type_ "range"
            , min_ "1"
            , max_ $ ms m
            , onInput $ f . fromMisoString
            ]
        ]

selectMenu :: [Attribute action] -> (a -> action) -> (MisoString -> action) -> [(MisoString, a)] -> View action
selectMenu as f e items =
    select_ (onChange (\s -> maybe (e $ "selectMenu: unrecognised value: " <> s) f $ Map.lookup s stringToItem) : as) $
        map (option_ [] . pure . text . fst) items
  where
    stringToItem = Map.fromList items

textArea :: [Attribute action] -> (MisoString -> action) -> MisoString -> View action
textArea as f t = textarea_ (onInput f : as) [text t]

{- Util -}

-- | Safe, wrapping around, as in 'relude'
next, prev :: (Eq a, Bounded a, Enum a) => a -> a
next e
    | e == maxBound = minBound
    | otherwise = succ e
prev e
    | e == minBound = maxBound
    | otherwise = pred e

newtype Class = Class {unClass :: MisoString}

renderHtml :: [Attribute action] -> SimpleDocTree [Class] -> View action
renderHtml as =
    let go = \case
            STEmpty -> [text ""]
            STChar c -> [text $ ms $ T.singleton c]
            STText _ t -> [text $ ms t]
            STLine i -> [br_ [], text $ ms $ T.replicate i $ T.singleton ' ']
            STAnn cs content -> [span_ [class_ $ Miso.unwords $ map unClass cs] $ go content]
            STConcat contents -> foldMap go contents
     in pre_ as . go
