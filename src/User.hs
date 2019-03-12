{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module User where

import Data.Text


data Move = Rock | Paper | Scissors deriving (Show)
data HandResult = Win | Draw | Lose deriving (Show)

resultHand :: Move -> Move -> HandResult
resultHand Rock Rock = Draw
resultHand Paper Paper = Draw
resultHand Scissors Scissors = Draw
resultHand Rock Scissors = Win
resultHand Scissors Paper = Win
resultHand Paper Rock = Win
resultHand _ _ = Lose

parseTextToMove :: Text -> Maybe Move
parseTextToMove "rock" = Just Rock
parseTextToMove "paper" = Just Paper
parseTextToMove "scissors" = Just Scissors
parseTextToMove _ = Nothing



type UserID = Text
data User = User { getMove :: IO Move 
                 , sendResponse :: Text -> IO () }
