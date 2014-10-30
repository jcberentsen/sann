module Client where

import Window

import Types (..)
import Action (..)
import Serialize (..)
import View (..)
import State (..)
import Inputs (..)

main : Signal Element
main = lift3 scene state alternativeContent Window.dimensions

state : Signal State
state = foldp step startingState action

step : Action -> State -> State
step action state =
    case action of
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
        PriorsUpdate ps -> { state | priors <- parsePriors ps }

        SampleChoice count -> { state | samples <- count }

        _ -> state

