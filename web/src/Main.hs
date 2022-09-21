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
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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
    mountPoint = Nothing -- Nothing defaults to 'body'
    logLevel = Off

updateModel :: Action -> Model -> Effect Action Model
updateModel = \case
    NoOp -> noEff
    Log t -> (<# (consoleLog t >> pure NoOp))
    TextEntered t -> noEff . set #inputText t
    OptsChanged f -> noEff . over #outputOptions f

-- TODO show initial values, without any flickering...
viewModel :: Model -> View Action
viewModel m =
    div_
        []
        [ div_
            [class_ "input"]
            [ textArea [class_ "input-text"] TextEntered ""
            , slider [conf, class_ "page-width"] 240 (OptsChanged . set #outputOptionsPageWidth) "Page width"
            , slider [conf, class_ "indentation"] 10 (OptsChanged . set #outputOptionsIndentAmount) "Indentation"
            , slider [conf, class_ "initial-indent"] 20 (OptsChanged . set #outputOptionsInitialIndent) "Initial indent"
            , checkBox [conf, class_ "compact"] (OptsChanged . set #outputOptionsCompact) "Compact"
            , checkBox [conf, class_ "compact-parens"] (OptsChanged . set #outputOptionsCompactParens) "Compact parentheses"
            , selectMenu [conf, class_ "string-style"] (OptsChanged . set #outputOptionsStringStyle) $
                Map.fromList
                    [ ("Literal", Literal)
                    , ("Escape non-printable", EscapeNonPrintable)
                    , ("Don't escape non-printable", DoNotEscapeNonPrintable)
                    ]
            ]
        , pPrintStringHtml [class_ "output-text"] (outputOptions m) . fromMisoString $ inputText m
        , link_
            [ rel_ "stylesheet"
            , href_ "style.css"
            ]
        ]
  where
    conf = class_ "config"

data ParensLevel
    = Parens0
    | Parens1
    | Parens2
    deriving (Eq, Show, Bounded, Enum)

pPrintStringHtml :: [Attribute act] -> OutputOptions -> String -> View act
pPrintStringHtml as opts = renderHtml as . treeForm . annotateWithIndentation . layoutStringAbstract opts
  where
    annotateWithIndentation ds = evalState (traverse f ds) $ prev Parens0
    f ann =
        (++ [Class "annotation", toClassName @Annotation ann]) <$> case ann of
            Open -> modify next *> g
            Close -> g <* modify prev
            Comma -> g
            _ -> pure []
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

selectMenu :: [Attribute action] -> (a -> action) -> Map MisoString a -> View action
selectMenu as f m =
    select_ (onChange (f . fromMaybe (error "selectMenu: unrecognised value") . (m !?)) : as)
        . map (option_ [] . pure . text)
        $ Map.keys m

textArea :: [Attribute action] -> (MisoString -> action) -> MisoString -> View action
textArea as f t = textarea_ (onInput f : as) [text t]

{- Util -}

-- | As in 'relude'
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
