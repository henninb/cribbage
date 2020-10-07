module Main where
-- module Main (makeCard) where
-- module Cards (standardDeck, deal, scoreHand, scoreCrib, hisNibs) where

import System.Random
import Data.List
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Applicative

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Read)
instance Show Suit where
  show Hearts = "H" ; show Diamonds = "D" ; show Clubs  = "C" ; show Spades = "S"

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Ord, Enum, Read)
instance Show Rank where
   show Ace   = "A" ; show Two  = "2" ;  show Three  = "3" ; show Four  = "4"
   show Five  = "5" ; show Six  = "6" ;  show Seven  = "7" ; show Eight  = "8"
   show Nine  = "9" ; show Ten = "T" ; show Jack   = "J" ; show Queen   = "Q" ; show King   = "K"

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Ord)
instance Show Card where
  show (Card r s) = show r ++ show s
instance Read Card where
  readsPrec _ cs = [(Card r s, cs'')]
    where (r, cs') = head . reads $ cs
          (s, cs'') = head . reads $ cs'

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
fifteen cards | (sum . map value $ cards) == 15 = 2
fifteen _ = 0

pair :: [Card] -> Int
pair [card1, card2] | rank card1 == rank card2 = 2
pair _ = 0

run :: [Card] -> Int
run cs | length cs >= 3 && run' cs = length cs
  where run' (c1:c2:cs) = v c2 == v c1 + 1 && run' (c2:cs)
        run' _ = True
        v = fromEnum . rank
run _ = 0

flush :: Hand -> Int
flush (c:cs) | all (\x -> suit x == s) cs = length cs + 1
  where s = suit c
flush _ = 0

hisNobs :: Card -> Hand -> Int
hisNobs s = length . filter (\card -> suit card == suit s && rank card == Jack)

hisNibs :: Card -> Int
hisNibs (Card Jack _) = 2
hisNibs _ = 0

scoreTheHand :: Bool -> Card -> Hand -> Int
scoreTheHand isCrib theCutCard hand =
  (sum . map scoreSet . sets $ (theCutCard:hand))
    + flush (theCutCard:hand)
    + hisNobs theCutCard hand
    + if isCrib then 0 else flush hand
  where scoreSet cs = sum . map (\f -> f cs) $ [fifteen, pair, run]
        sets = tail . sort . sets'
        sets' [] = [[]]
        sets' (x:xs) = sets' xs ++ map (x:) (sets' xs)

pickRandomCard :: [a] -> IO a
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
newDeck = [Card x y | y <- [Clubs .. Spades], x <- [Ace .. King]]

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

makeCard :: String -> Card
makeCard = read

makeHand :: String -> Hand
makeHand = map read . words

showCard :: Card -> String
showCard (Card n s) = show n ++ " -- " ++ show s

randomCard :: IO ()
randomCard = randomRank >>= \rank -> randomSuit >>= \suit -> print (getCard rank suit)

randomRank :: IO Int
randomRank = randomRIO (0,12)

randomSuit :: IO Int
randomSuit = randomRIO (0,3)

getCard :: Int -> Int -> Card
getCard rank suit = Card ([Ace .. King] !! rank) ([Clubs .. Spades] !! suit)

createCard :: Rank ->  Suit -> Card
createCard = Card

fullDeck :: Deck
fullDeck = [ Card j i | i <- [Clubs .. Spades], j <- [Ace .. King] ]

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

--extractSuit :: Card -> Suit
--extractSuit (Card a b) = b

--extractRank :: Card -> Rank
--extractRank (Card a b) = a

--allEqual :: Eq a => [a] -> Bool
--allEqual [] = True
--allEqual (x:xs) = all (== x) xs

--isFlush :: Hand -> CutCard -> Bool -> Bool
--isFlush hand cutCard isCrib = if isCrib then allEqual suitList && cutCardMatches else allEqual suitList
--  where
--    suitList = map suit hand
--    suitOfFistCard = head suitList
--    suitOfCutCard = suit cutCard
--    cutCardMatches = suitOfFistCard == suitOfCutCard
--
combination [] = [[]]
combination (x:xs) = combination xs ++ map (x:) (combination xs)

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
  let myHand = sort (dealFourCards shuffledDeck)
  print myHand
  putStrLn "--- separated ---"
  let cutCard = flipCutCard shuffledDeck
  print cutCard
  putStrLn "--- separated ---"
  let fourClubs = makeCard "Four Clubs"
  let fourDiamonds = makeCard "Four Diamonds"
  let fiveDiamonds = makeCard "Five Diamonds"
  let sixHeats = makeCard "Six Hearts"
  let aceHearts = makeCard "Ace Hearts"
  let jackDiamonds = makeCard "Jack Diamonds"
  let aceDiamonds = makeCard "Ace Diamonds"
  let totals = scoreTheHand False cutCard myHand
  let nobs = hisNobs cutCard myHand
  let comb = combination myHand
  print myHand
  print cutCard
  print totals
  print nobs
  print comb
