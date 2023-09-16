module Main where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (toJsonString)
import Data.Array (cons, uncons)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
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
      el "div" ["class" := "card-header"] [renderCommand cmd],
      el "div" ["class" := "card-body"] [
        case output of
          Nothing ->
            el "span" [] [text "..."]
          Just o ->
            renderOutput o
      ]
    ]

renderCommand :: Command -> Html
renderCommand = case _ of
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
  Ps ->
    el "span" [] [text "ps"]
  Launch package ->
    el "span" [] [
      text "launch ",
      el "strong" [] [text package]
    ]
  Kill id ->
    el "span" [] [
      text "kill ",
      el "strong" [] [text id]
    ]
  Res id ->
    el "span" [] [
      text "res ",
      el "strong" [] [text id]
    ]

renderOutput :: Output -> Html
renderOutput = case _ of
  Err e ->
    el "span" ["class" := "text-danger"] [text $ message e]
  Packages px ->
    el "table" ["class" := "table"] [
      el "thead" [] [
        el "tr" [] [
          el "th" [] [text "Package"]
        ]
      ],
      el "tbody" [] $
        px <#> \p ->
          el "tr" [] [
            el "td" [] [text p]
          ]
    ]
  Removed package ->
    el "span" [] [
      text "Removed ",
      el "strong" [] [text package]
    ]
  Installed package ->
    el "span" [] [
      text "Installed ",
      el "strong" [] [text package]
    ]
  Instances instances ->
    el "table" ["class" := "table"] [
      el "thead" [] [
        el "tr" [] [
          el "th" [] [text "ID"],
          el "th" [] [text "Package"],
          el "th" [] [text "State"]
        ]
      ],
      el "tbody" [] $
        instances <#> \{ id, package, state } ->
          el "tr" [] [
            el "td" [] [text id],
            el "td" [] [text package],
            el "td" [] [text $ show state]
          ]
    ]
  Launched id ->
    el "span" [] [
      text "Launched ",
      el "strong" [] [text id]
    ]
  Killed id ->
    el "span" [] [
      text "Killed ",
      el "strong" [] [text id]
    ]
  Output output ->
    el "span" [] [
      el "strong" [] [text $ fromMaybe "-" output]
    ]

--
--
--

data Command
  = Ls
  | Rm String
  | Install String
  | Ps
  | Launch String
  | Kill String
  | Res String

data Output
  = Err Error
  | Packages (Array String)
  | Removed String
  | Installed String
  | Instances (Array InstanceInfo)
  | Launched String
  | Killed String
  | Output (Maybe String)

type InstanceInfo = {
  id :: String,
  package :: String,
  state :: InstanceState
}

data InstanceState
  = Running
  | Terminated

instance Show InstanceState where
  show = case _ of
    Running ->
      "Running"
    Terminated ->
      "Terminated"

instance DecodeJson InstanceState where
  decodeJson j = do
    s <- decodeJson j
    case s of
      "Running" ->
        pure Running
      "Terminated" ->
        pure Terminated
      _ ->
        Left $ UnexpectedValue (encodeJson s)

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
  ["ps"] ->
    pure Ps
  ["launch", package] ->
    pure (Launch package)
  ["kill", id] ->
    pure (Kill id)
  ["res", id] ->
    pure (Res id)
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
  Ps ->
    Instances <$> ps
  Launch package ->
    Launched <$> launch package
  Kill id -> do
    kill id
    pure (Killed id)
  Res id ->
    Output <$> res id

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

ps :: Aff (Array InstanceInfo)
ps = rpc "/ps" unit

launch :: String -> Aff String
launch = rpc "/launch" <<< { package: _ }

kill :: String -> Aff Unit
kill = rpc "/kill" <<< { id: _ }

res :: String -> Aff (Maybe String)
res = rpc "/res" <<< { id: _ }

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
