module Player where

import Card

data Player = Player { name :: String, hand :: [Card], score :: Int } deriving (Show)

newPlayer :: String -> Player
newPlayer n = Player n [] 0
