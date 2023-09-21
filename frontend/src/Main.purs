module Main where

import Prelude

import Command (Command(..), parseCommand, renderCommand)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (forever)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Encode (toJsonString)
import Data.Array (cons, uncons)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
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
import Output (AttachmentConnectedState, AttachmentState(..), InstanceInfo, Output(..), renderOutput)
import Web.HTML (window)
import Web.HTML.Location (host)
import Web.HTML.Window (location)
import WebSocket (WebSocket, websocket, onError, onMessage, send)

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

--
--
--





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
    output <- runCommand id setHistory command
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

runCommand :: Int -> ((Array ExecutionState -> Array ExecutionState) -> Effect Unit) -> Command -> Aff Output
runCommand cmdId setHistory cmd = map (either Err identity) $ try $ case cmd of
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
  Launch package args ->
    Launched <$> launch package args
  Kill id -> do
    kill id
    pure (Killed id)
  Res id ->
    Output <$> res id
  Attach package -> do
    socket <- attach package (setHistory <<< pushAttachmentOutput cmdId) (setHistory <<< setOutput cmdId <<< Attached <<< ConnectionFailed)
    pure $ Attached $ Connected {
      socket,
      input: "",
      setInput: setHistory <<< setAttachmentInput cmdId,
      output: ""
    }

pickFile :: Aff String
pickFile = toAffE pickFile_

foreign import pickFile_ :: Effect (Promise String)

pushAttachmentOutput :: Int -> String -> Array ExecutionState -> Array ExecutionState
pushAttachmentOutput id output history = case uncons history of
  Nothing ->
    []
  Just { head, tail } ->
    if head.id == id
    then
      cons
        (head { output = withAttachedConnected (\props -> props { output = props.output <> output }) <$> head.output })
        tail
    else
      cons head (pushAttachmentOutput id output tail)

setAttachmentInput :: Int -> String -> Array ExecutionState -> Array ExecutionState
setAttachmentInput id input history = case uncons history of
  Nothing ->
    []
  Just { head, tail } ->
    if head.id == id
    then
      cons
        (head { output = withAttachedConnected (_ { input = input }) <$> head.output })
        tail
    else
      cons head (setAttachmentInput id input tail)

withAttachedConnected :: (AttachmentConnectedState -> AttachmentConnectedState) -> Output -> Output
withAttachedConnected f = case _ of
  Attached (Connected props) ->
    Attached (Connected $ f props)
  o ->
    o

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

launch :: String -> Array String -> Aff String
launch package args = rpc "/launch" { package, args }

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

--
--
--

type AttachProps = {
  setInput :: String -> Effect Unit,
  pushOutput :: String -> Effect Unit,
  onError :: String -> Effect Unit
}

attach :: String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Aff WebSocket
attach package pushOutput onErr = do
  hostname <- liftEffect getHost
  socket <- websocket $ "ws://" <> hostname <> "/attach/" <> package
  liftEffect do
    onError socket (onErr "Socket error")
    onMessage socket pushOutput
  void $ fork (prod socket)
  pure socket

prod :: WebSocket -> Aff Unit
prod socket = forever do
  delay (wrap 100.0)
  liftEffect $ send socket ""

getHost :: Effect String
getHost = host =<< location =<< window
