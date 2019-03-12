{-# LANGUAGE MultiParamTypeClasses #-}
module Event.ToEvent where

import Data.Text

class ToEvent a where
    decode :: Text -> Maybe a
    encode :: a -> Text




