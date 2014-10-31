module Inputs where

import Debug (watch)

import WebSocket (connect)
import String
import Window
import Graphics.Input
import Graphics.Input as Input
import Keyboard
import Maybe

import Graphics.Input.Field
import Graphics.Input.Field as Field

import Types (..)
import Action (..)
import Serialize (..)
import State (..)

eventurl = "ws://localhost:8000/socket"

events : Signal String
events = connect eventurl events_to_server

events_to_server : Signal String
events_to_server =
    (\action -> action :: [] |> watch "action to server" |> encodeServerActions) <~ serverActions

serverActions : Signal Action
serverActions = merges [menuInput.signal, samplesInput.signal, alternativeActions]

action : Signal Action
action = merges
    [ menuInput.signal
    , samplesInput.signal
    , alternativeActions
    , lift parseAction events
    ]

menuInput : Input.Input Action
menuInput = Input.input NoOp

clicks : Input.Input ()
clicks = Input.input ()

alternativeInput : Input.Input Field.Content
alternativeInput = Input.input Field.noContent

alternativeContent : Signal Field.Content
alternativeContent = merge alternativeInput.signal (always Field.noContent <~ entered)

probabilityInput : Input.Input Field.Content
probabilityInput = Input.input Field.noContent

probabilityContent : Signal Field.Content
probabilityContent = probabilityInput.signal

probabilities : Signal Float
probabilities = ((.string) >> defaultParseFloat 0.5) <~ probabilityInput.signal

defaultParseFloat : Float -> String -> Float
defaultParseFloat def s = case (String.toFloat s) of
    Nothing -> def
    Just f -> f

alternativeActions : Signal Action
alternativeActions = sampleOn entered (lift2 AddPrior tokens probabilities)

tokens : Signal String
tokens = .string <~ alternativeInput.signal

samplesInput : Input.Input Action
samplesInput = Input.input NoOp

{-| Signal that updates when the enter key is pressed. We will use it to sample
other signals. Actual value of this signal is not important.
-}
entered : Signal ()
entered = always () <~ keepIf identity True Keyboard.enter
