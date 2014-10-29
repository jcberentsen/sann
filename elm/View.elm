module View where

import Debug (watch)

import Graphics.Input as Input
import Graphics.Input.Field as Field

import Types (..)
import Action (..)
import State (..)
import Inputs (..)

scene : State -> Field.Content -> (Int, Int) -> Element
scene state alternativeContent (w,h) =
    container w h middle ((state, alternativeContent) |> watch "state" |> view)

view : (State, Field.Content) -> Element
view (state, alternativeContent) =
        flow down
            [ renderMenu state.model_menu
            , flow right [alternativeField alternativeContent, samplesMenu]
            , renderProbabilityDensity
            , (renderPotentials state.potentials)
            , (renderModel state.model)
            , (renderPopulation state.population)
            ]

renderModel : Model -> Element
renderModel m = case m of
    Ignorance -> ignorant
    Evidently es -> causal_node (map evidenceName es) []
    Causally c e -> causal_node [evidenceName c] [evidenceName e]
    AnyCause c e -> causal_node (map evidenceName c) [evidenceName e]
    AllCause c e -> flow right [node blue "all", causal_node (map evidenceName c) [evidenceName e]]
    Multiple cs -> flow down (map renderModel cs)
    _ -> ignorant

renderMenu : [String] -> Element
renderMenu items = Input.dropDown menuInput.handle (zip items (map ModelChoice items))

renderPotentials : [Potential] -> Element
renderPotentials pots = flow right (map renderPotential pots)

renderProbabilityDensity : Element
renderProbabilityDensity = pieChart [(plainText "rain", 0.5), (plainText "sprinklers", 0.1)]

pieChart : [(Element, Float)] -> Element
pieChart els =
    let fracs = map snd els
        offsets = scanl (+) 0 fracs
    in  collage 200 200 <|
        concat (zipWith3 (pieSlice 100) colors offsets els)

pieSlice : Float -> Color -> Float -> (Element, Float) -> [Form]
pieSlice radius colr offset (node, angle) =
    let makePoint t = fromPolar (radius, degrees (360 * offset + t))
    in  [ filled colr << polygon <| (0,0) :: map makePoint[ 0 .. 360 * angle ]
        , toForm (flow down [node, (asPercent angle)]) |> move (fromPolar (radius*0.7, turns (offset + angle/2)))
        ]

colors : [Color]
colors =
    [ lightBlue, lightGreen, lightYellow, lightRed
    , lightPurple, blue, green, yellow, red, purple
    ]

asPercent : Float -> Element
asPercent fraction =
      plainText <| show (toFloat (truncate (fraction * 100))) ++ "%"

renderPotential : Potential -> Element
renderPotential p = node green p

renderPopulation : Population -> Element
renderPopulation (Pop ps) = flow down (map (\s -> flow right (map population_node s)) ps)

samplesMenu : Element
samplesMenu = Input.dropDown samplesInput.handle (map (\n -> (show n, SampleChoice n)) [2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 50, 100, 1000, 10000])

causal_node : [String] -> [String] -> Element
causal_node causes effects =
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

node : Color -> String -> Element
node c name =
    let el = plainText name
        inner = color c (container (10 + widthOf el) 20 middle el)
    in
        color charcoal (container (2 + widthOf inner) (2 + heightOf inner) middle inner)

ignorant : Element
ignorant = node grey "?"

population_node : (Evidence, Ratio) -> Element
population_node (Evidence e v, r) =
    let tag = node blue e
        (w,h) = sizeOf tag
        col = case v of
            True -> green
            _ -> brown
        element = color col (container (10+w) (10+h) middle tag)
    in
        element

alternativeField : Field.Content -> Element
alternativeField = Field.field Field.defaultStyle alternative_input.handle identity "Alternative"

