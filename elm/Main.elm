module Main where

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

