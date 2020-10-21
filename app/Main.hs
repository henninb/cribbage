

module Main where

import Cribbage
import Controller

main :: IO ()
main = do
  putStrLn "--- separated ---"
  shuffledDeck <- dealCards (shuffleNew newDeck)
  print shuffledDeck
  putStrLn "--- separated ---"
  let cutCard = flipCutCard shuffledDeck
  print cutCard
  putStrLn "--- separated ---"
  let twoPlayers = dealTwoPlayers shuffledDeck 4
  let handOfPlayer1 = fst twoPlayers
  let handOfPlayer2 = snd twoPlayers
  mapM_ print [handOfPlayer1, handOfPlayer2]
  print (scoreTheHand False cutCard handOfPlayer1)
  print (scoreTheHand False cutCard handOfPlayer2)
  apiService
