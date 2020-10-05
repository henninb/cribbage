{-# LANGUAGE TupleSections #-}
module Main where

-- import Lib
import System.Random
import Data.List
-- import Data.Array.IO
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import Control.Applicative
-- import Control.Monad.State

-- data Suit = Club | Diamond | Heart | Spade   deriving (Show, Enum)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Show, Read)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show, Read)

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

mkRands = mapM (randomRIO . (,) 0) . enumFromTo 1 . pred

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l =
  let (a, b) = splitAt i l
  in a ++ c : drop 1 b

swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i, j) xs
  | i == j = xs
  | otherwise = replaceAt j (xs !! i) $ replaceAt i (xs !! j) xs

shuffle :: [a] -> IO [a]
shuffle xs = liftM (foldr swapElems xs . zip [1 ..]) (mkRands (length xs))
-- data Value = Two | Three | Four | Five | Six | Seven
--           | Eight | Nine | Ten | Jack | Queen
--           | King | Ace  deriving (Show, Enum)
--
-- shuffle :: [a] -> IO [a]
-- shuffle [] = return []
-- shuffle items = do
--   i <- randomTo $ (length items) - 1
--   let (firstHalf, (item:secondHalf)) = splitAt i items
--   (:) item <$> shuffle (firstHalf ++ secondHalf)

data Card = Card Rank Suit
  deriving (Eq, Show, Read)

-- type Card = (Suit, Rank)
type Deck = [Card]

type Hand = [Card]

oneCard :: IO ()
oneCard = randomRank >>= \rank -> randomSuit >>= \suit -> print (getCard rank suit)

randomCard :: IO ()
randomCard = randomRank >>= \rank -> randomSuit >>= \suit -> print (getCard rank suit)

randomRank :: IO Int
randomRank = randomRIO (0,12)

randomSuit :: IO Int
randomSuit = randomRIO (0,3)

getCard :: Int -> Int -> Card
getCard r s = Card ([Two .. Ace] !! r) ([Clubs .. Spades] !! s)


fullDeck :: Hand
fullDeck = [ Card j i | i <- [Clubs .. Spades], j <- [Two .. Ace] ]

deleteFromDeck :: Card -> Hand -> Hand
deleteFromDeck c = filter (== c)

addCard :: Card -> [Card] -> [Card]
addCard x xs = x:xs

-- makeDeck :: Deck
-- -- makeDeck = liftM2 (,) [Club ..] [Two ..]
-- makeDeck = concatMap (\suit -> map (suit,) [Two ..]) [Clubs ..]

-- randomList :: Int -> [Int]
-- randomList 0 = return []
-- randomList n = do
--   r  <- lift $ randomRIO (1,51)
--   rs <- lift $ randomList (n-1)
--   return (r:rs)


-- countTransactionsList:: [Int] -> Int
-- countTransactionsList = foldr (\ x -> (+) 1) 0

-- x = mapM_ putStrLn

randomCards :: Int -> IO[Int]
randomCards 0 = return []
randomCards n = do
  r  <- randomRIO (1,51)
  rs <- randomCards (n-1)
  return (r:rs)
-- shuffle :: Deck -> Deck
-- shuffle (Deck c) = Deck $ shuffleOnce randomN c
--     where   randomN = randomIO >>= (\x -> return (x `mod` 52))
--             shuffleOnce n c = (findNth n c : deleteNth n c)
--             findNth 0 (x:_) = x
--             findNth n (_:xs) = findNth (n-1) xs
--             deleteNth 0 (_:xs) = xs
--             deleteNth n (x:xs) = x : (deleteNth (n-1) xs)
-- foo <- randomRs (1,52) <$> newStdGen
-- let shuffle = shuffleRand foo

-- shuffle :: Deck -> Deck
-- shuffle (Deck c) = Deck $ shuffleOnce randomN c
--     where   randomN = randomIO >>= (\x -> return (x `mod` 52))
--             shuffleOnce n c = (findNth n c : deleteNth n c)
--             findNth 0 (x:_) = x
--             findNth n (_:xs) = findNth (n-1) xs
--             deleteNth 0 (_:xs) = xs
--             deleteNth n (x:xs) = x : (deleteNth (n-1) xs)

-- shuffle :: [a] -> IO [a]
-- shuffle xs = do
--         ar <- newArray n xs
--         forM [1..n] $ \i -> do
--             j <- randomRIO (i,n)
--             vi <- readArray ar i
--             vj <- readArray ar j
--             writeArray ar j vi
--             return vj

-- main :: IO ()
-- main = someFunc

main :: IO ()
main = do
  putStrLn "--- separated ---"
  -- print makeDeck
  -- mapM_ print ["1","2","3","4"]
  print fullDeck
  putStrLn "--- separated ---"
  randomCard
  putStrLn "--- separated ---"
  -- shuffle fullDeck
  -- print =<< randomList
  -- liftM $ print randomList
  -- mapM print randomList
  -- print (liftM (!! 1) randomList)
  -- mapM print randomList
  -- putStrLn $ show randomList
  -- mapM_ putStrLn randomList
  -- print randomList



-- getShuffledDeck :: Int -> [Int]
-- getShuffledDeck 0 = return []
-- getShuffledDeck n = do
--   r  <- randomRIO (0,51)
--   rs <- getShuffledDeck (n-1)
--   return (r:rs)

-- shuffledDeck = getShuffledDeck

-- getTopCard = 1

-- cardRankList = ['A', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K']
-- cardSuitList = ['H', 'D', 'C', 'S']
-- cardValueList = [1, 2, 3, 4, 5, 6, 7, 8, 8, 10, 10, 10, 10]

--let n = runRandom (randR(1, length (deck))) gen :: Int

--n = randR (1, 51 [1..51])

-- deckLength = length deck

-- firstCard = deck !! 4 `mod` 4
-- secondCard = deck !! 5 `mod` 4
-- thirdCard = deck !! 6 `mod` 4
-- forthCard = deck !! 7 `mod` 4

-- listBySuit = [x `mod` 4 | x <- deck]
-- listByRank = [x `div` 4 + 1 | x <- deck]
--listGetSuit = [x * 4 | x <- [0..51]]

