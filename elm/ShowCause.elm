import Debug (watch)

import WebSocket (connect)
import Json
import Maybe

eventurl = "ws://chrberenbox.rd.tandberg.com:8000/socket"

events_to_server : Signal String
events_to_server = constant ""

events : Signal String
events = connect eventurl events_to_server

step : String -> Element
step msg = msg |> Json.fromString |> (watch "msg") |> Maybe.maybe "{}" (Json.toString "") |> plainText

main : Signal Element
main = lift step events
