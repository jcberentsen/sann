{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Snap
import qualified Network.WebSockets.Snap as WSS
import qualified Network.WebSockets as WS
import Control.Concurrent

import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import           Data.Aeson                          (encode)

import Population ()
import Model
import Evidence

model :: CausalModel Text Bool
--model = rain_or_sprinklers
model = rain_causes_wet_model

wet_causes_slippery :: CausalModel Text Bool
wet_causes_slippery = Causally (fact "wet") (fact "slippery")

multi_model :: CausalModel Text Bool
multi_model = Multiple [Evidently [fact "rain"], rain_or_sprinklers, wet_causes_slippery]

ignorance :: CausalModel Text Bool
ignorance = Ignorance

rain_causes_wet_model :: CausalModel Text Bool
rain_causes_wet_model = Causally (fact "rain") (fact "wet")

rain_or_sprinklers :: CausalModel Text Bool
rain_or_sprinklers = AnyCause [fact "rain", fact "sprinklers"] (fact "wet")

site :: Snap ()
site = writeText "Hello!"

config :: Config Snap a
config = setAccessLog ConfigNoLog (setErrorLog ConfigNoLog defaultConfig)

main :: IO ()
main =
    httpServe (setPort 8000 config) $
        route
            [ ("socket", socket)
            ]
        Snap.<|>
        site

socket :: Snap ()
socket = WSS.runWebSocketsSnap wsApp

wsApp :: WS.ServerApp
wsApp pendingConnection = do
    connection <- WS.acceptRequest pendingConnection
    -- _msg <- WS.receiveData connection
    WS.sendTextData connection $ encode $ ignorance
    WS.sendTextData connection $ encode $ model
    WS.sendTextData connection $ encode $ rain_or_sprinklers
    WS.sendTextData connection $ encode $ multi_model
    keepAlive connection

keepAlive :: WS.Connection -> IO ()
keepAlive connection = do
    WS.sendPing connection $ BSC.pack "ping"
    threadDelay $ 10 * (1000000) -- 10 seconds
    keepAlive connection

