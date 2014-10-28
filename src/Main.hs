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

import Examples.MontyHall
import Examples.DihybridCross

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
    { session_model :: CausalModel Text Bool
    , session_alternatives :: Alternatives Text Bool
    , session_tosses :: Int
    }

data ServerAction
    = AddAlternative Text
    | SampleChoice Int
    | ModelChoice Text
    | NoOp
    deriving (Show)

$(deriveJSON defaultOptions ''ServerAction)

multi_model :: CausalModel Text Bool
multi_model = Multiple [rain_or_sprinklers, wet_causes_slippery]

rain_or_sprinklers :: CausalModel Text Bool
rain_or_sprinklers = AnyCause [rain, fact "sprinklers"] wet

wet_causes_slippery :: CausalModel Text Bool
wet_causes_slippery = Causally (wet) (fact "slippery")

rain = fact "rain"
wet = fact "wet"

no_population :: [[Evidence Text Bool]]
no_population = []

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

no_potentials :: Alternatives Text Bool
no_potentials = Alternatives []

models :: [(Text, CausalModel Text Bool)]
models =
    [ ("weather", multi_model)
    , ("MontyHall stay", staying_game)
    , ("MontyHall switch", switching_game)
    , ("green eyes", devo)
    , ("devo brown eyes", devo_brown)
    ]

modelMenu :: Actions Text Bool
modelMenu = ModelMenu $ map fst models

wsApp :: WS.ServerApp
wsApp pendingConnection = do
    connection <- WS.acceptRequest pendingConnection
    WS.sendTextData connection $ encode modelMenu
    WS.sendTextData connection $ encode $ ModelUpdate multi_model
    talk connection $ Session multi_model no_potentials 2

talk :: WS.Connection -> Session -> IO ()
talk connection session = do
    msg <- WS.receiveData connection
    let decoded = decode msg
    session' <-
        case decoded of
            Just actions -> foldM advance session actions

            _ -> do
                putStrLn $ "Could not decode from client"
                putStrLn $ show msg
                return session

    talk connection session'

    where
        advance session action =
            let (Session model alts tosses) = session
            in
            case action of
                AddAlternative "" -> return session
                AddAlternative alt -> do
                    putStrLn $ show action
                    let toggled = toggle_alternative (fact alt) alts
                    let alternatively = Alternatively toggled :: Potential Text Float Bool
                    let population = generate_population tosses alternatively model
                    WS.sendTextData connection $ encode $ PotentialUpdate $ toggled
                    WS.sendTextData connection $ encode $ PopulationUpdate $ (map observations_toList population)
                    return $ session { session_alternatives = toggled, session_tosses=3 }

                SampleChoice tosses -> do
                    putStrLn $ show action
                    let alternatively = Alternatively alts :: Potential Text Float Bool
                    let population = generate_population tosses alternatively model
                    WS.sendTextData connection $ encode $ PopulationUpdate $ (map observations_toList population)
                    return $ session { session_tosses=tosses }

                ModelChoice model_name -> do
                    putStrLn $ show action
                    let model' = maybe model id $ lookup model_name models
                    let session' = session { session_model = model', session_alternatives=no_potentials }
                    WS.sendTextData connection $ encode $ ModelUpdate model'
                    WS.sendTextData connection $ encode $ PotentialUpdate $ no_potentials
                    WS.sendTextData connection $ encode $ PopulationUpdate $ no_population
                    return $ session'

                _ -> return session

