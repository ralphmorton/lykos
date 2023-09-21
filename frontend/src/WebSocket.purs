module WebSocket (
  WebSocket,
  websocket,
  onError,
  onMessage,
  send
) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

--
--
--

foreign import data WebSocket :: Type

--
--
--

websocket :: String -> Aff WebSocket
websocket = toAffE <<< websocket_

foreign import websocket_ :: String -> Effect (Promise WebSocket)

--
--
--

foreign import onError :: WebSocket -> Effect Unit -> Effect Unit

--
--
--

foreign import onMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit

--
--
--

foreign import send :: WebSocket -> String -> Effect Unit
