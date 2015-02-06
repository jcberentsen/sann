{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE
      OverloadedStrings
    , GADTs
    , DeriveDataTypeable
    , DeriveGeneric
    , TemplateHaskell
    #-}

module Main where

import Model
import Evidence
import Likelyhood
import Population
import Observations

import Examples.MontyHall
import Examples.DihybridCross

import Snap
import qualified Network.WebSockets.Snap as WSS
import qualified Network.WebSockets as WS

import Data.Text (Text)
import           Data.Aeson                          (encode, decode)
import Data.Maybe
import Data.List (foldl')

import Data.Aeson.TH

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
    { sessionModel :: CausalModel Text Bool
    , sessionAlternatives :: Alternatives Text Bool
    , sessionPriors :: [Likelyhood Text Double]
    , sessionTosses :: Int
    }

data ServerAction
    = AddAlternative Text
    | AddPrior Text Double
    | SampleChoice Int
    | ModelChoice Text
    | NoOp
    deriving (Show)

$(deriveJSON defaultOptions ''ServerAction)

multiModel :: CausalModel Text Bool
multiModel = Multiple [rainOrSprinklers, wetCausesSlippery]

rainOrSprinklers :: CausalModel Text Bool
rainOrSprinklers = AnyCause [rain, fact "sprinklers"] wet

wetCausesSlippery :: CausalModel Text Bool
wetCausesSlippery = Causally wet (fact "slippery")

rain, wet :: Evidence Text Bool
rain = fact "rain" :: Evidence Text Bool
wet = fact "wet"

noPopulation :: [[Evidence Text Bool]]
noPopulation = []

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

noPotentials :: Alternatives Text Bool
noPotentials = Alternatives []

noPriors :: [Likelyhood Text Double]
noPriors = []

models :: [(Text, CausalModel Text Bool)]
models =
    [ ("weather", multiModel)
    , ("MontyHall stay", stayingGame)
    , ("MontyHall switch", switchingGame)
    , ("green eyes", devo)
    ]

modelMenu :: Actions
modelMenu = ModelMenu $ map fst models

wsApp :: WS.ServerApp
wsApp pendingConnection = do
    connection <- WS.acceptRequest pendingConnection
    WS.sendTextData connection $ encode modelMenu
    WS.sendTextData connection $ encode $ ModelUpdate multiModel
    let initialSession = Session multiModel noPotentials noPriors 2
    talk connection initialSession

talk :: WS.Connection -> Session -> IO ()
talk connection session = do
    msg <- WS.receiveData connection
    let decoded = decode msg
    session' <-
        case decoded of
            Just actions -> do
                let session'@(Session model _alts priors tosses) = foldl' advance session actions
                WS.sendTextData connection $ encode $ ModelUpdate model
                let potential = Likely priors
                let population = generatePopulation tosses potential model
                let populationList = map observationsToList population
                let populationSummary = summarizePopulation population
                WS.sendTextData connection $ encode $ PriorsUpdate priors
                WS.sendTextData connection $ encode $ PopulationUpdate $ groupCount populationList
                WS.sendTextData connection $ encode $ PopulationSummary populationSummary
                return session'

            _ -> do
                putStrLn "Could not decode from client"
                print msg
                return session

    talk connection session'

    where
        advance s action =
            case action of
                AddAlternative "" -> s
                AddPrior "" _ -> s

                AddPrior alt p -> do
                    let priors' = Likelyhood alt (P p) : sessionPriors session
                    s { sessionPriors = priors' }

                AddAlternative _ -> s

                SampleChoice tosses -> s { sessionTosses=tosses }

                ModelChoice modelName -> do
                    let model' = fromMaybe (sessionModel s) $ lookup modelName models
                    s {
                          sessionModel = model'
                        , sessionAlternatives = noPotentials
                        , sessionPriors = noPriors
                        }

                _ -> s
