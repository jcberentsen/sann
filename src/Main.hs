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
import           Data.Aeson                          (encode, decode)

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

data Session = Session
    { alternatives :: Alternatives Text Bool
    , samples :: Int }

data ServerAction
    = AddAlternative Text
    | SampleCount Int
    deriving (Show)

$(deriveFromJSON defaultOptions ''ServerAction)

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

talk :: WS.Connection -> Session -> IO ()
talk connection session = do
    let model = multi_model
    msg <- WS.receiveData connection
    let action = decode msg

    let alts = alternatives session
    alts' <- case action of
        Just (AddAlternative alt) -> do
            putStrLn $ show action
            let toggled = toggle_alternative (fact alt) alts
            let alternatively = Alternatively toggled :: Potential Text Float Bool
            let population = generate_population (samples session) alternatively model
            WS.sendTextData connection $ encode $ PotentialUpdate $ toggled
            WS.sendTextData connection $ encode $ PopulationUpdate $ (map observations_toList population)
            return toggled
        _ -> do
            putStrLn $ "Could not decode from client"
            putStrLn $ show msg
            return alts

    talk connection session { alternatives = alts' }

