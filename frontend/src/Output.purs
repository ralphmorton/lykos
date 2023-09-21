module Output (
  Output(..),
  InstanceInfo,
  InstanceState(..),
  AttachmentState(..),
  AttachmentConnectedState,
  renderOutput
) where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Exception (Error, message)
import Muon (Html, el, eventKey, eventTargetValue, on, text, (:=))
import WebSocket (WebSocket, send)

--
--
--

data Output
  = Err Error
  | Packages (Array String)
  | Removed String
  | Installed String
  | Instances (Array InstanceInfo)
  | Launched String
  | Killed String
  | Output (Maybe String)
  | Attached AttachmentState

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

data AttachmentState
  = ConnectionFailed String
  | Connected AttachmentConnectedState
  | Detached String

type AttachmentConnectedState = {
  socket :: WebSocket,
  input :: String,
  setInput :: String -> Effect Unit,
  output :: String
}

--
--
--

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
    el "textarea"
      [
        "rows" := "10",
        "class" := "form-control w-100 mb-3",
        "disabled" := "true"
      ]
      [text $ fromMaybe "-" output]
  Attached state -> case state of
    ConnectionFailed reason ->
      el "span" ["class" := "text-danger"] [
         text "Connection failed: ",
          el "strong" [] [text reason]
      ]
    Connected props ->
      el "div" [] [
        el "textarea"
          [
            "rows" := "10",
            "class" := "form-control w-100 mb-3",
            "disabled" := "true"
          ]
          [text props.output],
        el "input" [
          "class" := "form-control w-100",
          "type" := "text",
          "value" := props.input,
          on "input" (traverse_ props.setInput <<< eventTargetValue),
          on "keydown" \evt -> when (eventKey evt == pure "Enter") do
            send props.socket props.input
            props.setInput ""
        ] []
      ]
    Detached output ->
      el "textarea mb-3"
        [
          "cols" := "5",
          "class" := "form-control w-100",
          "disabled" := "true"
        ]
        [text output]
