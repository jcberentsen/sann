import Debug (watch)

import WebSocket (connect)
import Json
import Maybe
import String

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
        [toForm (Maybe.maybe ignorant render v)]

render : Json.Value -> Element
render v = ignorant

sketch : Maybe Json.Value -> Element
sketch v =
    collage 800 800
        [ toForm ignorant
        , move (100,0) (toForm fact)
        , move (280, 10) (toForm counterfact)
        , move (10, -160) (toForm (anycause ["rain"] ["wet"]))
        , move (300, -130) (toForm (anycause ["sprinklers", "rain"] ["wet"]))
        , move (-50, 0) (toForm (anycause ["gravity"] ["falling"]))
        , move (350, 100) (toForm population)
        ]

node : Color -> String -> Element
node c name =
    let el = plainText name
    in
        color c (container (10 + widthOf el) 20 middle el)

ignorant : Element
ignorant = node grey "?"

fact : Element
fact = node green "rain!"

counterfact : Element
counterfact = node red "no sprinklers!"

anycause : [String] -> [String] -> Element
anycause causes effects =
    let causes' = flow right (map (node yellow) causes)
        effects' = flow right (map (node orange) effects)
        maxw = max (widthOf causes') (widthOf effects')
        height = heightOf causes'
        lay = flow down
            [ container maxw height middle causes'
            , container maxw height middle effects'
            ]
    in
        color blue (container (10+maxw) (10+2*height) middle lay)

population : Element
population =
    let tag = node blue "eyecolor"
        (w,h) = sizeOf tag
        element = color brown (container (10+w) (10+h) middle tag)
    in
        element

main : Signal Element
main = lift step events
