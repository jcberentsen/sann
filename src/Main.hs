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

data Actions =
      PotentialUpdate (Alternatives Text Bool)
    | PriorsUpdate [Likelyhood Text Double]
    | ModelUpdate (CausalModel Text Bool)
    | ModelMenu [Text]
    | PopulationUpdate [([Evidence Text Bool], Int)]
    | PopulationSummary [(Text, PopulationRatio)]

type PopulationRatio = Double

$(deriveToJSON defaultOptions ''Actions)

data Session = Session
    { session_model :: CausalModel Text Bool
    , session_alternatives :: Alternatives Text Bool
    , session_priors :: [Likelyhood Text Double]
    , session_tosses :: Int
    }

data ServerAction
    = AddAlternative Text
    | AddPrior Text Double
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

no_priors = [] :: [Likelyhood Text Double]
--no_priors = [Likelyhood "rain" (P 0.5), Likelyhood "sprinklers" (P 0.1)] :: [Likelyhood Text Float]

models :: [(Text, CausalModel Text Bool)]
models =
    [ ("weather", multi_model)
    , ("MontyHall stay", staying_game)
    , ("MontyHall switch", switching_game)
    , ("green eyes", devo)
    ]

modelMenu :: Actions
modelMenu = ModelMenu $ map fst models

wsApp :: WS.ServerApp
wsApp pendingConnection = do
    connection <- WS.acceptRequest pendingConnection
    WS.sendTextData connection $ encode modelMenu
    WS.sendTextData connection $ encode $ ModelUpdate multi_model
    let initial_session = Session multi_model no_potentials no_priors 2
    talk connection initial_session

talk :: WS.Connection -> Session -> IO ()
talk connection session = do
    msg <- WS.receiveData connection
    let decoded = decode msg
    session' <-
        case decoded of
            Just actions -> do
                session'@(Session model alts priors tosses) <- foldM advance session actions
                WS.sendTextData connection $ encode $ ModelUpdate model
                let potential = Likely priors
                let population = generate_population tosses potential model
                let population_list = map observations_toList population
                let population_summary = summarizePopulation population
                WS.sendTextData connection $ encode $ PriorsUpdate $ priors
                WS.sendTextData connection $ encode $ PopulationUpdate $ groupCount population_list
                WS.sendTextData connection $ encode $ PopulationSummary population_summary
                return session'

            _ -> do
                putStrLn $ "Could not decode from client"
                putStrLn $ show msg
                return session

    talk connection session'

    where
        advance session action =
            return $ case action of
                AddAlternative "" -> session
                AddPrior "" _ -> session

                AddPrior alt p -> do
                    let priors' = (Likelyhood alt (P p)) : (session_priors session)
                    session { session_priors = priors' }

                AddAlternative alt -> do
                    session

                SampleChoice tosses -> do
                    session { session_tosses=tosses }

                ModelChoice model_name -> do
                    let model' = maybe (session_model session) id $ lookup model_name models
                    session {
                          session_model = model'
                        , session_alternatives=no_potentials
                        , session_priors=no_priors
                        }

                _ -> session
