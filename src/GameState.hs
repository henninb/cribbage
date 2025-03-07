module GameState where

import Card
import Player
import Deck

data GameState = GameState { player :: Player, computer :: Player, deck :: Deck, crib :: [Card], cutCard :: Maybe Card } deriving (Show)

initialGameState :: String -> Deck -> GameState
initialGameState playerName deck = GameState (newPlayer playerName) (newPlayer "Computer") deck [] Nothing
