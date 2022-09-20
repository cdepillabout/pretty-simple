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
import Lens.Micro (Lens', set)
import Miso.String (MisoString, fromMisoString, ms, toLower)
import qualified Miso.String as Miso
import Prettyprinter.Render.Util.SimpleDocTree (SimpleDocTree (..), treeForm)
import Text.Pretty.Simple (OutputOptions, StringOutputStyle (..), defaultOutputOptionsNoColor)
import Text.Pretty.Simple.Internal (Annotation (..), layoutString')

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

data Action where
    NoOp :: Action
    Log :: MisoString -> Action
    TextEntered :: MisoString -> Action
    OptsChanged :: Lens' OutputOptions a -> a -> Action

main :: IO ()
main = runApp $ startApp App{..}
  where
    initialAction = NoOp
    model =
        Model
            { inputText = example
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
    OptsChanged l x -> noEff . set (#outputOptions . l) x

-- TODO show initial values, without any flickering...
viewModel :: Model -> View Action
viewModel m =
    div_
        []
        [ div_
            [class_ "input"]
            [ textArea [class_ "input-text"] TextEntered example
            , slider [conf, class_ "page-width"] 240 (OptsChanged #outputOptionsPageWidth) "Page width"
            , slider [conf, class_ "indentation"] 10 (OptsChanged #outputOptionsIndentAmount) "Indentation"
            , slider [conf, class_ "initial-indent"] 20 (OptsChanged #outputOptionsInitialIndent) "Initial indent"
            , checkBox [conf, class_ "compact"] (OptsChanged #outputOptionsCompact) "Compact"
            , checkBox [conf, class_ "compact-parens"] (OptsChanged #outputOptionsCompactParens) "Compact parentheses"
            , selectMenu [conf, class_ "string-style"] (OptsChanged #outputOptionsStringStyle) $
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
pPrintStringHtml as opts = renderHtml as . treeForm . annotateWithIndentation . layoutString' opts
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

{- Example inputs -}

example, example1, example2, example3 :: MisoString
example = head [example1, example2, example3]
-- from https://github.com/cdepillabout/pretty-simple/issues/43
example1 =
    mconcat
        [ "AST [] [Def ((3,1),(5,30)) (Id \"fact'\" \"fact'\") [Equation ((4,1),(4,13)) () [PBox ((4,7),(4,9)) () "
        , "PConstr ((4,8),(4,8)) () (Id \"Z\" \"Z\") []] (Val ((4,13),(4,13)) () (NumInt 1)),Equation ((5,1),(5,30)) () "
        , "[PBox ((5,7),(5,11)) () (PConstr ((5,8),(5,10)) () (Id \"S\" \"S\") [PVar ((5,10),(5,10)) () "
        , "(Id \"m\" \"m_1\")])] (Binop ((5,30),(5,30)) () \"*\" (App ((5,15),(5,27)) () (Val ((5,15),(5,15)) () (Var () "
        , "(Id \"natToInt\" \"natToInt\"))) (App ((5,25),(5,27)) () (Val ((5,25),(5,25)) () (Constr () (Id \"S\" \"S\") "
        , "[])) (Val ((5,27),(5,27)) () (Var () (Id \"m\" \"m_1\"))))) (App ((5,32),(5,40)) () (Val ((5,32),(5,32)) () "
        , "(Var () (Id \"fact'\"\"fact'\"))) (Val ((5,38),(5,40)) () (Promote () (Val ((5,39),(5,39)) () (Var () "
        , "(Id \"m\" \"m_1\")))))))] (Forall ((3,9),(3,26)) [((Id \"n\" \"n_0\"),KPromote (TyCon (Id \"Nat\" \"Nat\")))] "
        , "(FunTy (Box (CInterval {lowerBound = CNat 1, upperBound = CVar (Id \"n\" \"n_0\")}) (TyApp (TyCon "
        , "(Id \"N\" \"N\")) (TyVar (Id \"n\" \"n_0\")))) (TyCon (Id \"Int\" \"Int\"))))]"
        ]
example2 = "Example 1 (\"text\", [] )"
-- from https://github.com/cdepillabout/pretty-simple/issues/64
example3 = ms $ show ("\DC1\205N\237\232s\225\232N\147K\173\RSE\201\EM" :: String)

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
