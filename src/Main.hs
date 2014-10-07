{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Snap
import qualified Network.WebSockets.Snap as WSS
import qualified Network.WebSockets as WS
import Control.Concurrent

import qualified Data.ByteString.Char8 as BSC

import Population ()

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

socket :: Snap ()
socket = WSS.runWebSocketsSnap wsApp

wsApp :: WS.ServerApp
wsApp pendingConnection = do
    connection <- WS.acceptRequest pendingConnection
    keepAlive connection

keepAlive :: WS.Connection -> IO ()
keepAlive connection = do
    WS.sendPing connection $ BSC.pack "ping"
    threadDelay $ 10 * (1000000) -- 10 seconds
    keepAlive connection

