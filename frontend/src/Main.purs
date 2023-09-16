module Main where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Encode (toJsonString)
import Data.Array (cons, uncons)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split, trim)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Muon (
  Html,
  Muon,
  Signal,
  el,
  eventKey,
  eventTargetValue,
  muon,
  on,
  text,
  state,
  (:=)
)

main :: Effect Unit
main = muon =<< app

app :: Effect (Signal (Muon Html))
app = do
  sig /\ cx <- state { input: "", idGen: 0, history: [] }
  pure $ sig <#> \{ input, idGen, history } -> do
    pure $
      el "main" ["class" := "h-100"] [
        el "div" ["class" := "container-xl py-5 d-flex align-items-end min-vh-100"] [
          el "div" ["class" := "flex-column w-100 text-start"] [
            el "div" [] (renderExecutionState <$> history),
            el "div" ["class" := "mt-3"] [
              el "input" [
                "class" := "form-control w-100",
                "type" := "text",
                "value" := input,
                on "input" (traverse_ (cx.input <<< const) <<< eventTargetValue),
                on "keydown" \evt -> when (eventKey evt == pure "Enter") do
                  traverse_ (run idGen cx.history) (eventTargetValue evt)
                  cx.idGen (_ + 1)
                  cx.input (const "")
              ] []
            ]
          ]
        ]
      ]

renderExecutionState :: ExecutionState -> Html
renderExecutionState { command, output } = case command of
  Left badInput ->
    el "div" ["class" := "card shadow mt-3"] [
      el "div" ["class" := "card-header"] [
        el "strong" [] [text badInput]
      ],
      el "div" ["class" := "card-body"] [el "span" ["class" := "text-warning"] [
        text "No such command"]
      ]
    ]
  Right cmd ->
    el "div" ["class" := "card shadow mt-3"] [
      el "div" ["class" := "card-header"] [
        case cmd of
          Ls ->
            el "span" [] [text "ls"]
          Rm package ->
            el "span" [] [
              text "rm ",
              el "strong" [] [text package]
            ]
          Install package ->
            el "span" [] [
              text "install ",
              el "strong" [] [text package]
            ]
      ],
      el "div" ["class" := "card-body"] [
        case output of
          Nothing ->
            el "span" [] [text "..."]
          Just (Err e) ->
            el "span" ["class" := "text-danger"] [text $ message e]
          Just (Packages px) ->
            el "div" [] $ px <#> \pkg ->
              el "div" [] [text pkg]
          Just (Removed package) ->
            el "span" [] [text $ "Removed " <> package]
          Just (Installed package) ->
            el "span" [] [text $ "Installed " <> package]
      ]
    ]

--
--
--

data Command
  = Ls
  | Rm String
  | Install String

data Output
  = Err Error
  | Packages (Array String)
  | Removed String
  | Installed String

type ExecutionState = {
  id :: Int,
  command :: Either String Command,
  output :: Maybe Output
}

run :: Int -> ((Array ExecutionState -> Array ExecutionState) -> Effect Unit) -> String -> Effect Unit
run id setHistory input = launchAff_ $ case parseCommand input of
  Just command -> do
    let st = { id, command: pure command, output: Nothing }
    liftEffect $ setHistory (_ <> [st])
    output <- runCommand command
    liftEffect $ setHistory (setOutput id output)
      
  Nothing -> do
    let st = { id, command: Left input, output: Nothing }
    liftEffect $ setHistory (_ <> [st])

setOutput :: Int -> Output -> Array ExecutionState -> Array ExecutionState
setOutput id output history = case uncons history of
  Nothing ->
    []
  Just { head, tail } ->
    if head.id == id
    then cons (head { output = pure output }) tail
    else cons head (setOutput id output tail)

parseCommand :: String -> Maybe Command
parseCommand input = case split (wrap " ") (trim input) of
  ["ls"] ->
    pure Ls
  ["rm", package] ->
    pure (Rm package)
  ["install", package] ->
    pure (Install package)
  _ ->
    Nothing

runCommand :: Command -> Aff Output
runCommand cmd = map (either Err identity) $ try $ case cmd of
  Ls ->
    Packages <$> ls
  Rm package -> do
    rm package
    pure (Removed package)
  Install package -> do
    base64 <- pickFile
    install package base64
    pure (Installed package)

pickFile :: Aff String
pickFile = toAffE pickFile_

foreign import pickFile_ :: Effect (Promise String)

--
--
--

ls :: Aff (Array String)
ls = rpc "/ls" unit

rm :: String -> Aff Unit
rm = rpc "/rm" <<< { package: _ }

install :: String -> String -> Aff Unit
install name base64 = rpc "/install" { name, base64 }

rpc :: forall a b. EncodeJson a => DecodeJson b => String -> a -> Aff b
rpc path body = do
  { json, status, text } <- fetch path {
    method: POST,
    headers: {
      "Accept": "application/json",
      "Content-Type": "application/json"
    },
    body: toJsonString body
  }
  if status < 300
  then
    fromJson json
  else do
    e <- text
    throwError (error e)
