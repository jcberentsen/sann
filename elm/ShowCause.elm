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
    , model_menu : [String]
    , potentials : [Potential]
    , samples : Int
    , population : Population
    }

startingState : State
startingState = State Ignorance [] [] 1 (Pop [])

data Action
    = NoOp
    -- from server
    | ModelUpdate Json.Value
    | ModelMenu Json.Value
    | PopulationUpdate Json.Value
    | PotentialUpdate Json.Value

    -- user input, to server
    | AddAlternative Potential
    | AddPrior String
    | SampleChoice Int
    | ModelChoice String

data Evidence = Evidence String Bool
data Model = Ignorance
           | Evidently [Evidence]
           | Causally Evidence Evidence
           | AnyCause [Evidence] Evidence
           | AllCause [Evidence] Evidence
           | Multiple [Model]

type Potential = String
no_potential : Potential
no_potential = ""

data Population = Pop [[(Evidence, Ratio)]]
type Ratio = (Int, Int)

parseAction : String -> Action
parseAction msg = case Json.fromString msg of
    Just v -> case v of
        Json.Object dict -> case Dict.get "tag" dict of
            Just (Json.String "ModelUpdate") -> Maybe.maybe NoOp ModelUpdate (Dict.get "contents" dict)
            Just (Json.String "PopulationUpdate") -> Maybe.maybe NoOp PopulationUpdate (Dict.get "contents" dict)
            Just (Json.String "PotentialUpdate") -> Maybe.maybe NoOp PotentialUpdate (Dict.get "contents" dict)
            Just (Json.String "ModelMenu") -> Maybe.maybe NoOp ModelMenu (Dict.get "contents" dict)
            _ -> NoOp

        _ -> NoOp
    _ -> NoOp

parsePotentials : Json.Value -> [Potential]
parsePotentials v = case v of
    (Json.Array pots) -> map parsePotential pots
    _ -> []

parsePotential : Json.Value -> Potential
parsePotential v = case v of
    (Json.Array [pot, p]) -> parseString pot
    _ -> ""

parsePopulation : Json.Value -> Population
parsePopulation v = case v of
    Json.Array vs -> Pop (map parsePopulationSet vs)
    _ -> Pop []

parsePopulationSet : Json.Value -> [(Evidence, Ratio)]
parsePopulationSet v = case v of
    (Json.Array vs) -> map parsePopulationPair vs

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

parseModelMenu : Json.Value -> [String]
parseModelMenu v = case v of
    Json.Array items -> map parseString items
    _ -> []

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
    "AllCause" -> Just (AllCause (get_evidence_array "_causes" dict) (get_evidence "_effect" dict))
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
    AllCause c e -> flow right [node blue "all", causal_node (map evidenceName c) [evidenceName e]]
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
main = lift3 scene state alternativeContent Window.dimensions

scene : State -> Field.Content -> (Int, Int) -> Element
scene state alternativeContent (w,h) =
    container w h middle ((state, alternativeContent) |> watch "state" |> view)

view : (State, Field.Content) -> Element
view (state, alternativeContent) =
        flow down
            [ renderMenu state.model_menu
            , flow right [alternativeField alternativeContent, samplesMenu]
            , (renderPotentials state.potentials)
            , (renderModel state.model)
            , (renderPopulation state.population)
            ]

state : Signal State
state = foldp step startingState action

steps : [Action] -> State -> State
steps actions state = actions |> watch "actions" |> foldl step state

step : Action -> State -> State
step action state =
    case (watch "action" action) of
        NoOp -> state

        ModelUpdate v ->
            { state | model <-
                case parseModel v of
                    Just mo -> mo
                    _ -> Ignorance
            }

        ModelMenu v ->
            { state | model_menu <- parseModelMenu v
            }

        PopulationUpdate v ->
            { state | population <- parsePopulation v
            }

        PotentialUpdate pots -> { state | potentials <- parsePotentials pots }

        SampleChoice count -> { state | samples <- count }

        _ -> state

eventurl = "ws://chrberenbox.rd.tandberg.com:8000/socket"

events : Signal String
events = connect eventurl events_to_server

events_to_server : Signal String
events_to_server =
    (\action -> action :: [] |> watch "action to server" |> encodeServerActions) <~ serverActions

serverActions : Signal Action
serverActions = merges [menuInput.signal, samplesActions, alternativeActions]

encodeServerActions : [Action] -> String
encodeServerActions actions = "[" ++ join ", " (map (\action -> case action of
    AddAlternative alt -> "{\"tag\":\"AddPrior\",\"contents\":\"" ++ alt ++ "\"}"
    AddPrior alt -> "{\"tag\":\"AddPrior\",\"contents\":\"" ++ alt ++ "\"}"
    ModelChoice modelname     -> "{\"tag\":\"ModelChoice\",\"contents\":\"" ++ modelname ++ "\"}"
    SampleChoice tosses       -> "{\"tag\":\"SampleChoice\",\"contents\":" ++ show tosses ++ "}"
    NoOp -> "{\"tag\":\"NoOp\",\"contents\":[]}"
    _ -> "{}") actions) ++ "]"

action : Signal Action
action = merges
    [ menuInput.signal
    , samplesActions
    , alternativeActions
    , lift parseAction events
    ]

menuInput : Input.Input Action
menuInput = Input.input NoOp

renderMenu : [String] -> Element
renderMenu items = Input.dropDown menuInput.handle (zip items (map ModelChoice items))

clicks : Input.Input ()
clicks = Input.input ()

alternative_input : Input.Input Field.Content
alternative_input = Input.input Field.noContent

alternativeContent : Signal Field.Content
alternativeContent = merge alternative_input.signal (always Field.noContent <~ entered)

alternativeField : Field.Content -> Element
alternativeField = Field.field Field.defaultStyle alternative_input.handle identity "Alternative"

alternativeActions : Signal Action
alternativeActions = AddAlternative <~ enteredAlternative

enteredAlternative : Signal String
enteredAlternative = dropIf String.isEmpty "" (sampleOn entered (.string <~ alternative_input.signal))

samplesInput : Input.Input Action
samplesInput = Input.input NoOp

samplesMenu : Element
samplesMenu = Input.dropDown samplesInput.handle (map (\n -> (show n, SampleChoice n)) [2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 50, 100, 1000, 10000])

samplesActions = samplesInput.signal

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
renderPopulation (Pop ps) = flow down (map (\s -> flow right (map population_node s)) ps)
