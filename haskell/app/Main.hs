{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (encode,decode)
import Data.String
import Data.Maybe (maybe)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified Network.Wai.Handler.Warp as Warp

import Messages
import Codec
import Controller

handleSocket :: WS.Connection -> IO ()
handleSocket conn = do
  let send msg = WS.sendTextData conn (encode (msg :: Command))
  -- Empfange die Daten vom Client
  msg <- WS.receiveData conn  
  mapM_ (control send) (decode msg)
  handleSocket conn

----------------------------------------------------------------------------

-- Die folgenden funktionen dienen dem generellen Setup der Server Anwendung
-- und sind für die Lösung der Aufgabe nicht relvant.


-- Der statische Teil der Web Anwendung (html + javascript)
static = Static.staticApp (staticSettings)
staticSettings = (Static.defaultFileServerSettings "../gui")
  { Static.ssIndices = [Static.unsafeToPiece "elevator.svg"] }

-- Der WebSocket
socket pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  handleSocket conn

-- Starte die Webanwendung auf Port 3000
main = Warp.run 3000 $
  WS.websocketsOr WS.defaultConnectionOptions socket static