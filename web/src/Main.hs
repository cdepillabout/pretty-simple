{-# LANGUAGE CPP #-}

module Main where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets (defaultConnectionOptions)
#endif

import Data.Generics.Labels ()
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro
import Miso hiding (go, set)
import Miso.String (MisoString, fromMisoString, ms)
import Prettyprinter.Render.Util.SimpleDocTree (SimpleDocTree (..), treeForm)
import Text.Pretty.Simple
import Text.Pretty.Simple.Internal (layoutString)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp app =
    Warp.runSettings (Warp.setPort 8000 $ Warp.setTimeout 3600 Warp.defaultSettings) =<<
        JSaddle.jsaddleOr defaultConnectionOptions (app >> syncPoint) JSaddle.jsaddleApp
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
main = runApp $ startApp App {..}
  where
    initialAction = NoOp
    model =
        Model
            { inputText = example
            , outputOptions = defaultOutputOptionsLightBg --TODO use dark background
            }
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing -- Nothing defaults to 'body'
    logLevel = Off

--TODO submit GHCJS/Miso bug
renderStyle :: Style -> View action -> View action
#ifdef __GHCJS__
renderStyle s = span_ [style_ $ Map.singleton "color" "blue"] . pure
#else
renderStyle Style {..} =
    --TODO use all fields
    (if styleBold then b_ [] . pure else id) . case styleColor of
        Nothing -> id
        Just (c, i) -> span_ [style_ $ uncurry Map.singleton $ renderColor c i] . pure
  where
    renderColor c _i = ("color", ms $ show c) --TODO use intensities (consult ANSI color chart)
#endif

updateModel :: Action -> Model -> Effect Action Model
updateModel = \case
    NoOp -> noEff
    Log t -> (<# (consoleLog t >> pure NoOp))
    TextEntered t -> noEff . set #inputText t
    OptsChanged l x -> noEff . set (#outputOptions . l) x

--TODO show initial values, without any flickering...
viewModel :: Model -> View Action
viewModel m =
    div_
        []
        [ textarea_ [onInput TextEntered] [text example]
        , div_
              []
              [ input_ [type_ "checkbox", onChecked $ OptsChanged #outputOptionsCompact . unChecked]
              , text "compact"
              ]
        , div_
              []
              [ input_ [type_ "checkbox", onChecked $ OptsChanged #outputOptionsCompactParens . unChecked]
              , text "compact parens"
              ]
        , slider 240 #outputOptionsPageWidth "Page width"
        , slider 10 #outputOptionsIndentAmount "Indentation"
        , slider 20 #outputOptionsInitialIndent "Initial indent"
        , selectMenu (OptsChanged #outputOptionsStringStyle) $
              Map.fromList
                  [ ("Literal", Literal)
                  , ("Escape non-printable", EscapeNonPrintable)
                  , ("Don't escape non-printable", DoNotEscapeNonPrintable)
                  ]
        , pPrintStringHtml (outputOptions m) . fromMisoString $ inputText m
        ]

slider :: Int -> (Lens' OutputOptions Int) -> MisoString -> View Action
slider m l t =
    div_
        []
        [ input_
              [ type_ "range"
              , min_ "1"
              , max_ $ ms m
              , onInput $ OptsChanged l . fromMisoString
              ]
        , text t
        ]

unChecked :: Checked -> Bool
unChecked (Checked b) = b

pPrintStringHtml :: OutputOptions -> String -> View act
pPrintStringHtml opts = renderHtml . fmap renderStyle . treeForm . layoutString opts

renderHtml :: SimpleDocTree (View act -> View act) -> View act
renderHtml =
    let go = \case
            STEmpty -> [text ""]
            STChar c -> [text $ ms $ T.singleton c]
            STText _ t -> [text $ ms t]
            STLine i -> [br_ [], text $ ms $ T.replicate i $ T.singleton ' ']
            STAnn ann content -> map ann $ go content
            STConcat contents -> foldMap go contents
     in pre_ [] . go

selectMenu :: (a -> action) -> Map MisoString a -> View action
selectMenu f m =
    select_ [onChange $ f . fromMaybe (error "selectMenu: unrecognised value") . (m !?)] $
        map (option_ [] . pure . text) $ Map.keys m

example, example1, example2, example3 :: MisoString
example = example1
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
