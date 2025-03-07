module GameLogic where

import Card
import Deck
import Player
import GameState

-- Deal cards to players and the crib
dealCards :: GameState -> GameState
dealCards (GameState p c d _ _) = let
    (playerHand, rest1) = splitAt 6 d
    (computerHand, rest2) = splitAt 6 rest1
    (cribHand, rest3) = splitAt 4 rest2
    cutCard = head rest3
    in GameState (p { hand = playerHand }) (c { hand = computerHand }) (tail rest3) cribHand (Just cutCard)

-- Play a round of Cribbage
playRound :: GameState -> IO GameState
playRound state = do
    -- Implement the logic for playing a round
    -- This includes drawing cards, scoring, and updating the game state
    return state
