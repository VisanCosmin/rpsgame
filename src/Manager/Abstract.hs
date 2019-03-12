{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Manager.Abstract where

import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.Map as M


newtype Manager s a = Manager { eventStream :: MVar a }



push :: Manager s a -> a -> IO ()
push man = putMVar (eventStream man) 

createManager :: (s -> a -> IO s) -> s -> IO (Manager s a) 
createManager step s0 = do eventStream <- newEmptyMVar
                            
                           let loop s = do ac <- takeMVar eventStream
                                           s' <- step s ac
                                           loop s'
                           forkIO $ loop s0
                           return $ Manager eventStream
						   

						    