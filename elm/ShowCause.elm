import Debug (watch)

import WebSocket (connect)
import Json
import Maybe
import String
import Dict
import Window
import Graphics.Input
import Graphics.Input as Input

import Graphics.Input.Field
import Graphics.Input.Field as Field

type State =
    { model : Model
    , potential : Potential
    , population : Population
    , input_content : Field.Content
    }

data Action
    = NoOp
    | ChangeModel Json.Value
    | UpdateInput Field.Content
    | UpdatePotential Potential

startingState : State
startingState =  State Ignorance NoPotential (Pop []) Field.noContent

data Evidence = Evidence String Bool
data Model = Ignorance
           | Evidently [Evidence]
           | Causally Evidence Evidence
           | AnyCause [Evidence] Evidence
           | Multiple [Model]

data Potential = NoPotential

data Population = Pop [(Evidence, Ratio)]
type Ratio = (Int, Int)

parseAction : String -> Action
parseAction msg = case Json.fromString msg of
    Just v -> ChangeModel v
    _ -> NoOp

parseModel : Json.Value -> Maybe Model
parseModel v = case v of
    Json.Object dict -> parseModelFromDict dict |> watch "model"
    _ -> Just Ignorance

evidenceFromArray : Json.Value -> Evidence
evidenceFromArray v = case v of
    (Json.Array [Json.String name, Json.Boolean v]) -> Evidence name v
    _ -> Evidence "" False

parseModelFromDict : Dict.Dict String Json.Value -> Maybe Model
parseModelFromDict dict = case Dict.get "tag" dict of
    Just (Json.String tag) -> parseModelTag tag dict
    _ -> Nothing

parseModelTag : String -> Dict.Dict String Json.Value -> Maybe Model
parseModelTag tag dict = case tag of
    "Ignorance" -> Just Ignorance
    "Evidently" -> Just (Evidently (get_evidence_array "_evidence" dict))
    "Causally" -> Just (Causally (get_array "_causer" dict |> evidenceFromArray) (get_array "_effect" dict |> evidenceFromArray))
    "AnyCause" -> Just (AnyCause (get_evidence_array "_causes" dict) (get_evidence "_effect" dict))
    "Multiple" -> Just (Multiple ((\(Json.Array ls) -> filterMap parseModel ls) (get_array "_causalities" dict)))
    _ -> Nothing

get_array : String -> Dict.Dict String Json.Value -> Json.Value
get_array name dict = Maybe.maybe (Json.Array []) (\a->a) (Dict.get name dict)

get_evidence_array : String -> Dict.Dict String Json.Value -> [Evidence]
get_evidence_array name dict = (\(Json.Array ls) -> map evidenceFromArray ls) (get_array name dict)

get_evidence : String -> Dict.Dict String Json.Value -> Evidence
get_evidence name dict = evidenceFromArray (get_array name dict)

evidenceName (Evidence name _) = name

renderModel : Model -> Element
renderModel m = case m of
    Ignorance -> ignorant
    Evidently es -> causal_node (map evidenceName es) []
    Causally c e -> causal_node [evidenceName c] [evidenceName e]
    AnyCause c e -> causal_node (map evidenceName c) [evidenceName e]
    Multiple cs -> flow down (map renderModel cs)
    _ -> ignorant


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
    in
        color c (container (10 + widthOf el) 20 middle el)

ignorant : Element
ignorant = node grey "?"

population : Element
population =
    let tag = node blue "eyecolor"
        (w,h) = sizeOf tag
        element = color brown (container (10+w) (10+h) middle tag)
    in
        element

main : Signal Element
main = lift3 scene state clicks.signal Window.dimensions

scene : State -> () -> (Int, Int) -> Element
scene state clicked (w,h) =
    container w h middle (view state)

view : State -> Element
view state =
        flow down
            [ potentialField state.input_content
            , (renderPotential state.potential)
            , (renderModel state.model)
            , (renderPopulation state.population)
            ]

state : Signal State
state = foldp step startingState actions

step : Action -> State -> State
step action state =
    case action of
        NoOp -> state
        ChangeModel v -> { state | model <- case parseModel v of
            Just mo -> mo
            _ -> Ignorance }
        UpdateInput content -> { state | input_content = content }
        UpdatePotential pot -> { state | potential = pot }

eventurl = "ws://chrberenbox.rd.tandberg.com:8000/socket"

events_to_server : Signal String
events_to_server = constant ""

events : Signal String
events = connect eventurl events_to_server

actions : Signal Action
actions = merges
    [ lift parseAction events
    , inputActions
    , potentialActions
    ]

clicks : Input.Input ()
clicks = Input.input ()

potential_input : Input.Input Field.Content
potential_input = Input.input Field.noContent

potentialField : Field.Content -> Element
potentialField = Field.field Field.defaultStyle potential_input.handle identity "Potential"

validatedPotentials : Signal Potential
validatedPotentials = lift (always NoPotential) potential_input.signal

potentialActions : Signal Action
potentialActions = lift UpdatePotential validatedPotentials

inputActions : Signal Action
inputActions = lift (\inp -> inp |> UpdateInput |> watch "input") potential_input.signal

renderPotential : Potential -> Element
renderPotential p = case p of
    _ -> ignorant -- potentialField

renderPopulation : Population -> Element
renderPopulation p = case p of
    _ -> ignorant
