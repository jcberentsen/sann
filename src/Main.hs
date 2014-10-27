{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE
      OverloadedStrings
    , GADTs
    , DeriveDataTypeable
    , DeriveGeneric
    , TemplateHaskell
    #-}

module Main where

import Snap
import qualified Network.WebSockets.Snap as WSS
import qualified Network.WebSockets as WS
import Control.Concurrent

import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text, empty)
import           Data.Aeson                          (encode)

import Data.Typeable
import GHC.Generics
import Data.Aeson.TH

import Model
import Evidence
import Likelyhood
import Population
import Observations

data Actions name p =
      PotentialUpdate (Alternatives name p)
    | ModelUpdate (CausalModel name p)
    | ModelMenu [Text]
    | PopulationUpdate [[Evidence name p]]

$(deriveToJSON defaultOptions ''Actions)

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

modelMenu :: Actions Text Bool
modelMenu = ModelMenu ["weather", "eyecolor", "Monty Hall", "faces"]

wsApp :: WS.ServerApp
wsApp pendingConnection = do
    connection <- WS.acceptRequest pendingConnection
    WS.sendTextData connection $ encode modelMenu
    WS.sendTextData connection $ encode $ ModelUpdate multi_model
    talk connection $ Session weather_potentials 2

data Session = Session
    { alternatives :: Alternatives Text Bool
    , samples :: Int }

talk :: WS.Connection -> Session -> IO ()
talk connection session = do
    let model = multi_model
    msg <- WS.receiveData connection
    let alts = alternatives session
    potentials' <- case msg of
        "" -> return $ alternatives session
        _ -> do
            putStrLn $ show (msg :: Text)
            let new_alt = fact msg
            let new_potentials = toggle_alternative new_alt alts
            let alt_potentials = Alternatively new_potentials :: Potential Text Float Bool
            let population = generate_population (samples session) alt_potentials model
            WS.sendTextData connection $ encode $ PotentialUpdate $ new_potentials
            WS.sendTextData connection $ encode $ PopulationUpdate $ (map observations_toList population)
            return new_potentials

    talk connection session { alternatives = potentials' }

