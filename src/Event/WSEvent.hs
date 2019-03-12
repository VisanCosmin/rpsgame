{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Event.WSEvent where

import Event.ToEvent
import Data.Text
import Text.ParserCombinators.ReadP



data Connect = Connect { userID :: Text } deriving ( Show )
data Pair = Pair deriving (Show , Read)

instance ToEvent Pair where
    encode Pair = " { event : Pair } "
    decode str = flip parse str $ string "{ event : pair }" >> return Pair

instance ToEvent Connect where
	encode Connect{..} = " { event : { connect : " `append` userID `append` " } } "
	decode str = justHead $ readP_to_S parseConnect $ unpack str
                 where parseConnect = do string "{ event : { connect : "
                                         userID <- many1 (satisfy $ const True)
                                         string " } } "
                                         return $ Connect $ pack userID


object :: ReadP a -> ReadP a
object parseObj =
    do char '{'
       obj <- parseObj
       char '}'
       return obj

parameter :: String -> ReadP a -> ReadP a
parameter label parsePara = 
    do string label
       char ':'
       parsePara



parse parser str = justHead $ readP_to_S (skipSpaces >> parser) $ unpack str 



justHead :: [(a,b)] -> Maybe a
justHead ((x,_):_) = Just x
justHead _ = Nothing