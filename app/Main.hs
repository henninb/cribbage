{-# LANGUAGE TupleSections #-}
module Main where

import Lib
import System.Random
import Data.List
-- import Data.Array.IO
import Control.Monad

data Suit = Club | Diamond | Heart | Spade   deriving (Show, Enum)

data Value = Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen
          | King | Ace  deriving (Show, Enum)

type Card = (Suit, Value)
type Deck = [Card]

makeDeck :: Deck
-- makeDeck = liftM2 (,) [Club ..] [Two ..]
makeDeck = concatMap (\suit -> map (suit,) [Two ..]) [Club ..]

randomList :: Int -> IO[Int]
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,51)
  rs <- randomList (n-1)
  return (r:rs)


countTransactionsList:: [Int] -> Int
countTransactionsList = foldr (\ x -> (+) 1) 0


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
  print makeDeck
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

