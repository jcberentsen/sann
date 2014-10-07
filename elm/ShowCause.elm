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
--step msg = msg |> Json.fromString |> (watch "msg") |> Maybe.maybe "{}" (Json.toString "") |> plainText
step msg = msg |> Json.fromString |> (watch "msg") |> visualize

visualize : Maybe Json.Value -> Element
visualize v =
    collage 800 800
        [ toForm ignorant
        , move (100,0) (toForm fact)
        , move (280, 10) (toForm counterfact)
        , move (10, -160) (toForm cause)
        ]

ignorant : Element
ignorant = color grey (container 20 20 middle (plainText "?"))

fact : Element
fact = color green (container 60 30 middle (plainText "rain!"))

counterfact : Element
counterfact = color red (container 160 30 middle (plainText "no sprinklers!"))

cause : Element
cause = flow down
    [ color yellow (container 60 30 middle (plainText "rain?"))
    , color orange (container 60 30 middle (plainText "wet?")) ]

main : Signal Element
main = lift step events
