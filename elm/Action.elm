module Action where

import Types (..)
import Json

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

