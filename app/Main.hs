

module Main where

import Cribbage
import Controller
import System.Exit
import System.Environment

import Card
import Deck
import Player
import GameState
import GameLogic

main :: IO ()
main = do
    putStrLn "Welcome to Cribbage!"
    putStrLn "Enter your name:"
    playerName <- getLine
    deck <- shuffleDeck fullDeck
    let initialState = initialGameState playerName deck
    finalState <- playRound initialState
    putStrLn "Game over!"
    print finalState
  -- putStrLn "--- separated ---"
  -- programName <- getProgName
  -- args <- getArgs
  -- if "-h" `elem` args || "--help" `elem` args then
  --   print (programName ++ " help") >> exitSuccess
  -- else if "-v" `elem` args || "--version" `elem` args then
  --   print (programName ++ "version") >> exitSuccess
  -- else
  --   print "running main program..."
  -- putStrLn "--- separated ---"
  -- shuffledDeck <- dealCards (shuffleNew newDeck)
  -- print shuffledDeck
  -- putStrLn "--- separated ---"
  -- let cutCard = flipCutCard shuffledDeck
  -- print cutCard
  -- putStrLn "--- separated ---"
  -- let twoPlayers = dealTwoPlayers shuffledDeck 4
  -- let handOfPlayer1 = fst twoPlayers
  -- let handOfPlayer2 = snd twoPlayers
  -- mapM_ print [handOfPlayer1, handOfPlayer2]
  -- print (scoreTheHand False cutCard handOfPlayer1)
  -- print (scoreTheHand False cutCard handOfPlayer2)
  -- apiService
