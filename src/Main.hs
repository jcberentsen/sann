{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Snap
import qualified Network.WebSockets.Snap as WSS
import qualified Network.WebSockets as WS
import Control.Concurrent

import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text, empty)
import           Data.Aeson                          (encode)

import Model
import Evidence
import Likelyhood
import Population
import Observations

multi_model :: CausalModel Text Bool
multi_model = Multiple [rain_or_sprinklers, wet_causes_slippery]

rain_or_sprinklers :: CausalModel Text Bool
rain_or_sprinklers = AnyCause [rain, fact "sprinklers"] wet

wet_causes_slippery :: CausalModel Text Bool
wet_causes_slippery = Causally (wet) (fact "slippery")

rain = fact "rain"
wet = fact "wet"

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

weather_potentials :: Alternatives Text Bool
weather_potentials = Alternatives []

wsApp :: WS.ServerApp
wsApp pendingConnection = do
    connection <- WS.acceptRequest pendingConnection
    WS.sendTextData connection $ encode $ multi_model
    talk connection weather_potentials
    --keepAlive connection

talk :: WS.Connection -> Alternatives Text Bool -> IO ()
talk connection potentials = do
    let model = multi_model
    msg <- WS.receiveData connection
    potentials' <- case msg of
        "" -> return potentials
        _ -> do
            putStrLn $ show (msg :: Text)
            let new_potential = alternatively (fact msg) potentials
            let alt_potential = Alternatively new_potential :: Potential Text Float Bool
            let population = generate_population 2 alt_potential model
            WS.sendTextData connection $ encode $ concat (map observations_toList population)
            return new_potential

    talk connection potentials'

{-
keepAlive :: WS.Connection -> IO ()
keepAlive connection = do
    WS.sendPing connection $ BSC.pack "ping"
    threadDelay $ 10 * (1000000) -- 10 seconds
    keepAlive connection
-}

