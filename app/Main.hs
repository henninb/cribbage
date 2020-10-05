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

data Card = Card Rank Suit
  deriving (Eq, Show, Read)

--type Card = (Suit, Rank)
type Deck = [Card]
type Hand = [Card]
type CutCard = [Card]

pickRandomCard :: [a] -> IO a
-- pickRandomCard xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
pickRandomCard xs = (xs !!) <$> randomRIO (0, length xs - 1)

mkRands :: Int -> IO [Int]
mkRands = mapM (randomRIO . (,) 0) . enumFromTo 1 . pred

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l =
 let (a, b) = splitAt i l
 in a ++ c : drop 1 b

swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i, j) xs
 | i == j = xs
 | otherwise = replaceAt j (xs !! i) $ replaceAt i (xs !! j) xs

shuffleNew :: [a] -> IO [a]
shuffleNew xs = fmap (foldr swapElems xs . zip [1 ..]) (mkRands (length xs))

newDeck :: Deck
newDeck = [Card x y | y <- [Clubs .. Spades], x <- [Two .. Ace]]

shuffle :: Deck -> IO Deck
shuffle deck =
    if not (null deck)
        then do
            let deckLen = length deck - 1
            n <- randomRIO(0, deckLen) :: IO Int
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
flipCutCard deck = [deck !! n | n <- [51]]

cutCard deck = head . deck 51

-- merge :: [a] -> [a] -> [a]
-- merge [] ys = ys
-- merge (x:xs) ys = x:merge ys xs

extractSuit :: Card -> Suit
extractSuit (Card a b) = b
--
extractRank :: Card -> Rank
extractRank (Card a b) = a

isFlush :: Hand -> CutCard -> Bool
isFlush [] a = True
isFlush (x:xs) a = (Spades == extractSuit x) && isFlush a xs

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
  let myCutCard = flipCutCard shuffledDeck
  print myCutCard
  putStrLn "--- separated ---"
  -- let x = merge myHand myCutCard
  let x = isFlush myHand myCutCard
  let y = createCard King Clubs
  print x
  print y
  -- let y = x !! 0 -- | n <- [0]
-- getTopCard = 1
