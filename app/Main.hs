

module Main where

import Cribbage

main :: IO ()
main = do
  putStrLn "--- separated ---"
  shuffledDeck <- dealCards (shuffleNew newDeck)
  --shuffledDeck <- dealCards (shuffle makeDeck)
  print shuffledDeck
  putStrLn "--- separated ---"
  let deckLength = length shuffledDeck
  print deckLength
  let myHand = (dealFourCards shuffledDeck)
  print myHand
  putStrLn "--- separated ---"
  let cutCard = flipCutCard shuffledDeck
  print cutCard
  putStrLn "--- separated ---"
  let totals = scoreTheHand False cutCard myHand
  print totals

