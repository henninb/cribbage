module Card where

-- data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)
-- data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Show)

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Eq, Show, Enum, Bounded)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Show, Enum, Bounded)
