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
model = multi_model

wet_causes_slippery :: CausalModel Text Bool
wet_causes_slippery = Causally (fact "wet") (fact "slippery")

multi_model :: CausalModel Text Bool
multi_model = Multiple [Evidently [fact "rain"], rain_or_sprinklers, wet_causes_slippery]

rain_or_sprinklers :: CausalModel Text Bool
rain_or_sprinklers = AnyCause [fact "rain", fact "sprinklers"] (fact "wet")

site :: Snap ()
site = writeText "Connect the 'ShowCause.elm' client to 'Sann'!"

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
    WS.sendTextData connection $ encode $ model
    --msg <- WS.receiveData connection
    keepAlive connection

keepAlive :: WS.Connection -> IO ()
keepAlive connection = do
    WS.sendPing connection $ BSC.pack "ping"
    threadDelay $ 10 * (1000000) -- 10 seconds
    keepAlive connection

