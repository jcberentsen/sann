import Debug (watch)

import WebSocket (connect)
import Json
import Maybe
import String
import Dict
import Window
import Graphics.Input
import Graphics.Input as Input
import Keyboard

import Graphics.Input.Field
import Graphics.Input.Field as Field

type State =
    { model : Model
    , potentials : [Potential]
    , population : Population
    , input_content : Field.Content
    }

data Action
    = NoOp
    | ChangeModel Json.Value
    | UpdatePopulation [Json.Value]
    | UpdateInput Field.Content
    | AddPotential Potential

startingState : State
startingState =  State Ignorance [] (Pop []) Field.noContent

data Evidence = Evidence String Bool
data Model = Ignorance
           | Evidently [Evidence]
           | Causally Evidence Evidence
           | AnyCause [Evidence] Evidence
           | Multiple [Model]

type Potential = String
no_potential : Potential
no_potential = ""

data Population = Pop [(Evidence, Ratio)]
type Ratio = (Int, Int)

parseAction : String -> Action
parseAction msg = case Json.fromString (msg |> watch "msg") of
    Just v -> case (v |> watch "value" |> identity) of
        Json.Array vals -> UpdatePopulation vals
        _ -> ChangeModel v
    _ -> NoOp

parsePopulation : [Json.Value] -> Population
parsePopulation vs = Pop (map parsePopulationPair vs)

parsePopulationPair : Json.Value -> (Evidence, Ratio)
parsePopulationPair v = case v of
    (Json.Array [ev, p]) -> (Evidence (parseString ev) (parseBool p), (1, 1))
    _ -> (Evidence "?" True, (0,0))

parseBool : Json.Value -> Bool
parseBool v =
    case v of
        Json.Boolean b -> b
        _ -> False

parseString : Json.Value -> String
parseString v =
    case v of
        Json.String s -> s
        _ -> ""

parseModel : Json.Value -> Maybe Model
parseModel v = case v of
    Json.Object dict -> parseModelFromDict dict
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

main : Signal Element
main = lift3 scene state clicks.signal Window.dimensions

scene : State -> () -> (Int, Int) -> Element
scene state clicked (w,h) =
    container w h middle (state |> watch "state" |> view)

view : State -> Element
view state =
        flow down
            [ potentialField state.input_content
            , (renderPotentials state.potentials)
            , (renderModel state.model)
            , (renderPopulation state.population)
            ]

state : Signal State
state = foldp step startingState actions

step : Action -> State -> State
step action state =
    case (action |> watch "action") of
        NoOp -> state

        ChangeModel v ->
            { state | model <-
                case parseModel v of
                    Just mo -> mo
                    _ -> Ignorance
            }
        UpdatePopulation v ->
            { state | population <- parsePopulation v
            }

        UpdateInput content -> { state | input_content <- content }

        AddPotential "" -> state

        AddPotential pot ->
            { state | potentials <- pot :: state.potentials
                    , input_content <- Field.noContent}

isAddPotential : Action -> Bool
isAddPotential action =
    case action of
        AddPotential _ -> True
        _ -> False

encodeServerAction : Action -> String
encodeServerAction _ = ""

eventurl = "ws://chrberenbox.rd.tandberg.com:8000/socket"

events : Signal String
events = connect eventurl events_to_server

events_to_server : Signal String
events_to_server =
    (\action ->
        case (action |> watch "action to server") of
            AddPotential "" -> ""
            AddPotential pot -> pot
    ) <~ serverActions

serverActions : Signal Action
serverActions = keepIf isAddPotential NoOp potentialActions

actions : Signal Action
actions = merges
    [ potentialActions
    , inputActions
    , lift parseAction events
    ]

clicks : Input.Input ()
clicks = Input.input ()

potential_input : Input.Input Field.Content
potential_input = Input.input Field.noContent

potentialField : Field.Content -> Element
potentialField = Field.field Field.defaultStyle potential_input.handle identity "Potential"

potentialActions : Signal Action
potentialActions = AddPotential <~ sampleOn entered (.string <~ potential_input.signal)

inputActions : Signal Action
inputActions = UpdateInput <~ potential_input.signal

{-| Signal that updates when the enter key is pressed. We will use it to sample
other signals. Actual value of this signal is not important.
-}
entered : Signal ()
entered = always () <~ keepIf identity True Keyboard.enter

renderPotentials : [Potential] -> Element
renderPotentials pots = flow right (map renderPotential pots)

renderPotential : Potential -> Element
renderPotential p = node green p

renderPopulation : Population -> Element
renderPopulation (Pop ps) = flow right (map population_node ps)
