module Action where

import Types (..)
import Json

data Action
    = NoOp
    -- from server
    | ModelUpdate Json.Value
    | ModelMenu Json.Value
    | PotentialUpdate Json.Value
    | PriorsUpdate Json.Value
    | PopulationUpdate Json.Value
    | PopulationSummary Json.Value

    -- user input, to server
    | AddAlternative Potential
    | AddPrior String Float
    | SampleChoice Int
    | ModelChoice String

