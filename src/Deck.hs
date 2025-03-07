module Deck where

import System.Random
import Card
import System.Random.Shuffle (shuffle')

type Deck = [Card]

-- fullDeck :: Deck
-- fullDeck = [Card s r | s <- [Hearts..Spades], r <- [Ace..King]]

fullDeck :: Deck
fullDeck = [Card s r | s <- [Hearts .. Spades], r <- [Ace .. King]]

shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
    gen <- newStdGen
    return $ shuffle' deck (length deck) gen
