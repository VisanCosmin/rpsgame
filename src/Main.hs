{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main where

import qualified Data.ByteString.Lazy.Internal as B


import Control.Exception (finally)
import Control.Monad (forM_, forever , when)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.STM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import Event.WSEvent
import Event.ToEvent
import Manager.RPS
import Manager.Abstract
import User

main :: IO ()
main = do
  putStrLn "Aplicatia a inceput"
  man <- createManager'
  
  WS.runServer "127.0.0.1" 9160 $ application man


application :: Manager ManagerState ManagerEvent -> WS.ServerApp
application man pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    moveStream <- newEmptyMVar

    userID <- (T.dropAround ('"'==)) <$> WS.receiveData conn :: IO T.Text
    print userID
    push man $ NewUser userID 
                       User { getMove = takeMVar moveStream 
                            , sendResponse = WS.sendTextData conn}
 

    let listen = do raw <- (T.dropAround ('"'==)) <$> WS.receiveData conn :: IO T.Text
                    print raw
                    when (raw == "pair") $ push man (PairUser userID)
                    case parseTextToMove raw of Nothing -> return ()
                                                (Just mv) -> putMVar moveStream mv

    --case decoded of Nothing -> print "nu e nimic"
    --                (Just x) -> print x


    --WS.sendTextData conn $ encode $ decodedMessage
    finally (forever listen) (push man (DisconnectUser userID))

first :: IO (Maybe a) -> IO a
first act = do ma <- act
               case ma of Nothing -> first act
                          (Just a) -> pure a