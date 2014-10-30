module Inputs where

import Debug (watch)

import WebSocket (connect)
import String
import Window
import Graphics.Input
import Graphics.Input as Input
import Keyboard

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

alternative_input : Input.Input Field.Content
alternative_input = Input.input Field.noContent

alternativeContent : Signal Field.Content
alternativeContent = merge alternative_input.signal (always Field.noContent <~ entered)

alternativeActions : Signal Action
alternativeActions = AddAlternative <~ enteredAlternative

enteredAlternative : Signal String
enteredAlternative = dropIf String.isEmpty "" (sampleOn entered (.string <~ alternative_input.signal))

samplesInput : Input.Input Action
samplesInput = Input.input NoOp

{-| Signal that updates when the enter key is pressed. We will use it to sample
other signals. Actual value of this signal is not important.
-}
entered : Signal ()
entered = always () <~ keepIf identity True Keyboard.enter
