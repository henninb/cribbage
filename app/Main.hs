module Main where

import System.Random
import Data.List
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Applicative

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Show, Read)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show, Read)
-- instance Show Rank where
--   show Ace   = "A" ; show Two  = "2" ;  show Three  = "3" ; show Four  = "4"
--   show Five  = "5" ; show Six  = "6" ;  show Seven  = "7" ; show Eight  = "8"
--   show Nine  = "9" ; show Ten = "10" ; show Jack   = "J" ; show Queen   = "Q" ; show King   = "K"

data Card = Card { rank :: Rank, suit :: Suit }
-- data Card = Card Rank Suit
  deriving (Eq, Show, Read)

--type Card = (Suit, Rank)
type Deck = [Card]
type Hand = [Card]
type CutCard = Card

value :: Card -> Int
value (Card Ace _) = 1
value (Card Jack _) = 10
value (Card Queen _) = 10
value (Card King _) = 10
value c = (fromEnum . rank $ c) + 2

fifteen :: [Card] -> Int
fifteen cs | (sum . map value $ cs) == 15 = 2
fifteen _ = 0

getCardValue :: [Rank] -> Maybe Int
getCardValue = elemIndex Three

pickRandomCard :: [a] -> IO a
-- pickRandomCard xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
pickRandomCard xs = (xs !!) <$> randomRIO (0, length xs - 1)

makeRandoms :: Int -> IO [Int]
makeRandoms = mapM (randomRIO . (,) 0) . enumFromTo 1 . pred

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l =
 let (a, b) = splitAt i l
 in a ++ c : drop 1 b

swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i, j) xs
 | i == j = xs
 | otherwise = replaceAt j (xs !! i) $ replaceAt i (xs !! j) xs

shuffleNew :: [a] -> IO [a]
shuffleNew xs = fmap (foldr swapElems xs . zip [1 ..]) (makeRandoms (length xs))

newDeck :: Deck
newDeck = [Card x y | y <- [Clubs .. Spades], x <- [Two .. Ace]]

shuffle :: Deck -> IO Deck
shuffle deck =
    if not (null deck)
        then do
            let deckLength = length deck - 1
            n <- randomRIO(0, deckLength) :: IO Int
            let randomCard = deck !! fromIntegral n
            tailShuffle <- shuffle (delete randomCard deck)
            return (randomCard : tailShuffle)
        else return deck


--makeDeck :: [Deck]
--makeDeck = liftM2 Card(,) [Clubs ..] [Two ..]
--makeDeck = concatMap (\suit -> map (suit,) [Two ..]) [Clubs ..]

showCard :: Card -> String
showCard (Card n s) = show n ++ " -- " ++ show s

randomCard :: IO ()
randomCard = randomRank >>= \rank -> randomSuit >>= \suit -> print (getCard rank suit)

randomRank :: IO Int
randomRank = randomRIO (0,12)

randomSuit :: IO Int
randomSuit = randomRIO (0,3)

getCard :: Int -> Int -> Card
getCard r s = Card ([Two .. Ace] !! r) ([Clubs .. Spades] !! s)

createCard :: Rank ->  Suit -> Card
createCard = Card

isSpade :: Card -> Bool
isSpade card = Spades == extractSuit card

isSuit :: Card -> Suit -> Bool
isSuit card suit = suit == extractSuit card

fullDeck :: Deck
fullDeck = [ Card j i | i <- [Clubs .. Spades], j <- [Two .. Ace] ]

deleteFromDeck :: Card -> Deck -> Deck
deleteFromDeck c = filter (== c)

addCard :: Card -> [Card] -> [Card]
addCard x xs = x:xs

dealCards :: Monad m => m Deck -> m Deck
dealCards d =
    do
    deck <- d
    return [deck !! n | n <- [0..51]]

dealFourCards :: Deck -> Hand
dealFourCards deck = [deck !! n | n <- [0..3]]

flipCutCard :: Deck -> CutCard
flipCutCard = last

-- merge :: [a] -> [a] -> [a]
-- merge [] ys = ys
-- merge (x:xs) ys = x:merge ys xs

extractSuit :: Card -> Suit
extractSuit (Card a b) = b
--
extractRank :: Card -> Rank
extractRank (Card a b) = a

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

isFlush :: Hand -> CutCard -> Bool -> Bool
isFlush hand cutCard isCrib = if isCrib then allEqual suitList && cutCardMatches else allEqual suitList
  where
    suitList = map extractSuit hand
    suitOfFistCard = head suitList
    suitOfCutCard = extractSuit cutCard
    cutCardMatches = suitOfFistCard == suitOfCutCard

main :: IO ()
main = do
  putStrLn "--- separated ---"
  -- print makeDeck
  -- mapM_ print ["1","2","3","4"]
  print fullDeck
  putStrLn "--- separated ---"
  randomCard
  putStrLn "--- separated ---"
  shuffledDeck <- dealCards (shuffleNew newDeck)
  --shuffledDeck <- dealCards (shuffle makeDeck)
  print shuffledDeck
  putStrLn "--- separated ---"
  let deckLength = length shuffledDeck
  print deckLength
  let myHand = dealFourCards shuffledDeck
  print myHand
  putStrLn "--- separated ---"
  let cutCard = flipCutCard shuffledDeck
  print cutCard
  putStrLn "--- separated ---"
  let x = isFlush myHand cutCard False
  let y = getCardValue [Three]
  print y
  print x

