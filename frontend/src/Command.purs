module Command (
  Command(..),
  renderCommand,
  parseCommand
) where

import Prelude

import Data.Array (filter, uncons)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (joinWith, split, trim)
import Muon (Html, el, text, (:=))

--
--
--

data Command
  = Ls
  | Rm String
  | Install String
  | Ps
  | Launch String (Array String)
  | Kill String
  | Res String
  | Attach String

--
--
--

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
  Launch package args ->
    el "span" [] [
      text "launch ",
      el "strong" [] [text $ package <> joinWith " " args]
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
  Attach package ->
    el "span" [] [
      text "attach ",
      el "strong" [] [text package]
    ]

--
--
--

parseCommand :: String -> Maybe Command
parseCommand input = do
  { head, tail } <- uncons $ filter (_ /= "") $ split (wrap " ") (trim input)
  case head, tail of
    "ls", [] ->
      pure Ls
    "rm", [package] ->
      pure (Rm package)
    "install", [package] ->
      pure (Install package)
    "ps", [] ->
      pure Ps
    "launch", rest -> do
      r <- uncons rest
      pure (Launch r.head r.tail)
    "kill", [id] ->
      pure (Kill id)
    "res", [id] ->
      pure (Res id)
    "attach", [package] ->
      pure (Attach package)
    _, _ ->
      Nothing
