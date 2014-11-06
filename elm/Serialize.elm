module Serialize where

import Json
import Maybe
import String
import Dict

import Types (..)
import Action (..)

parseAction : String -> Action
parseAction msg = case Json.fromString msg of
    Just v -> case v of
        Json.Object dict -> case Dict.get "tag" dict of
            Just (Json.String "ModelUpdate") -> Maybe.maybe NoOp ModelUpdate (Dict.get "contents" dict)
            Just (Json.String "PopulationUpdate") -> Maybe.maybe NoOp PopulationUpdate (Dict.get "contents" dict)
            Just (Json.String "PopulationSummary") -> Maybe.maybe NoOp PopulationSummary (Dict.get "contents" dict)
            Just (Json.String "PotentialUpdate") -> Maybe.maybe NoOp PotentialUpdate (Dict.get "contents" dict)
            Just (Json.String "PriorsUpdate") -> Maybe.maybe NoOp PriorsUpdate (Dict.get "contents" dict)
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

parsePriors : Json.Value -> [(String,Float)]
parsePriors v = case v of
    (Json.Array ps) -> map parsePrior ps
    _ -> []

parsePrior : Json.Value -> (String, Float)
parsePrior v = case v of
    (Json.Array [pot, p]) -> (parseString pot, parseFloat p)
    _ -> ("?",0.0)

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

parsePopulationSummary : Json.Value -> [(String, Float)]
parsePopulationSummary v = case v of
    Json.Array vs -> map parseDensity vs
    _ -> []

parseDensity : Json.Value -> (String, Float)
parseDensity v = case v of
    (Json.Array [name, p]) -> ((parseString name), (parseFloat p))
    _ -> ("?", 0.0)

parseBool : Json.Value -> Bool
parseBool v =
    case v of
        Json.Boolean b -> b
        _ -> False

parseFloat : Json.Value -> Float
parseFloat v =
    case v of
        Json.Number b -> b
        _ -> 1.0

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

-- Encode to Server
encodeServerActions : [Action] -> String
encodeServerActions actions = "[" ++ join ", " (map (\action -> case action of
    AddAlternative alt -> "{\"tag\":\"AddPrior\",\"contents\":\"" ++ alt ++ "\"}"
    AddPrior alt p -> "{\"tag\":\"AddPrior\",\"contents\":[\"" ++ alt ++ "\", " ++ show p ++ "]}"
    ModelChoice modelname     -> "{\"tag\":\"ModelChoice\",\"contents\":\"" ++ modelname ++ "\"}"
    SampleChoice tosses       -> "{\"tag\":\"SampleChoice\",\"contents\":" ++ show tosses ++ "}"
    NoOp -> "{\"tag\":\"NoOp\",\"contents\":[]}"
    _ -> "{}") actions) ++ "]"

