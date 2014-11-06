module State where

import Types (..)

type State =
    { model : Model
    , model_menu : [String]
    , potentials : [Potential]
    , priors : [(String, Float)]
    , samples : Int
    , population : Population
    , population_summary : [(String, Float)]
    }

startingState : State
startingState = State Ignorance [] [] [] 1 (Pop []) []
