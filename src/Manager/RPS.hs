{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Manager.RPS where

import Manager.Abstract

import Control.Concurrent.MVar
import Control.Concurrent
import Data.Text
import qualified Data.Map as M
import User


data ManagerState = ManagerState { users :: M.Map UserID User
                                 , pending :: Maybe UserID }

data ManagerEvent = NewUser UserID User
				          | PairUser UserID
				          | DisconnectUser UserID


update :: ManagerState -> ManagerEvent -> IO ManagerState
update ms@ManagerState{..} (NewUser id user) = 
    do sendResponse user $ quote "joined"
       return $ ms { users = M.insert id user users}

update ms@ManagerState{..} (PairUser p1) =
    do case pending of Nothing -> do case mUser of (Just user) -> do sendResponse user $ quote "pending"
                                                                     return $ ms { pending = Just p1 }
                                                   Nothing -> return ms
                                     where mUser = M.lookup p1 users
                       (Just p2) -> pairPlayers user1 user2

                                       where user1 = M.lookup p1 users
                                             user2 = M.lookup p2 users
                                             pairPlayers :: Maybe User -> Maybe User -> IO ManagerState
                                             pairPlayers (Just user1) Nothing 
                                                = return $ ms { pending = Just p1} 
                                             pairPlayers Nothing (Just user2) 
                                                = return $ ms { pending = Just p2}
                                             pairPlayers (Just user1) (Just user2) 
                                                = do sendResponse user1 $ quote $ "other " `append` p2
                                                     sendResponse user2 $ quote $ "other " `append` p1
                                                     
                                                     

                                                     print $ p1 `append` " vs " `append` p2 
                                                     forkIO $ playGame 2 user1 user2

                                                     return $ ms { pending = Nothing }



update ms@ManagerState{..} (DisconnectUser id) = 
    do putStrLn "Te-ai deconectat"
       case pending of Nothing -> return $ ms { users = M.delete id users }
                       (Just x) -> if x == id then return $ ms { users = M.delete id users  , pending = Nothing}
                                              else return $ ms { users = M.delete id users }



createManager' = createManager update s0
                  where s0 = ManagerState { users = M.empty
                                          , pending = Nothing }


playGame :: Int -> User -> User -> IO ()
playGame n user1 user2 = do sendResponse user1 $ quote "gamestart"
                            sendResponse user2 $ quote "gamestart"

                            round 0 0

                         where round :: Int -> Int -> IO ()
                               round x _ | x == n = do sendResponse user1 $ quote "win"
                                                       sendResponse user2 $ quote "lose"
                                                       threadDelay 1500000
                                                       sendResponse user1 $ quote $ "ready"
                                                       sendResponse user2 $ quote $ "ready"
                               round _ y | y == n = do sendResponse user2 $ quote "win"
                                                       sendResponse user1 $ quote "lose"
                                                       threadDelay 1500000
                                                       sendResponse user1 $ quote $ "ready"
                                                       sendResponse user2 $ quote $ "ready"

                               round x y = do mv1 <- getMove user1
                                              mv2 <- getMove user2
                                              sendResponse user1 $ quote $ pack $ show mv2
                                              sendResponse user2 $ quote $ pack $ show mv1  

                                              threadDelay 1500000
                                              sendResponse user1 $ quote $ "ready"
                                              sendResponse user2 $ quote $ "ready"

                                              case resultHand mv1 mv2 of Win -> round (x+1) y
                                                                         Draw -> round x y
                                                                         Lose -> round x (y+1)



quote :: Text -> Text
quote a = "\""  `append` a `append` "\""

