{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Char
import Lib

-- each index represents a line of the board, the value in each
-- index represents the number of artifacts in the current board.
board :: [Int]
board = [1, 3, 5, 7]

-- print the current state of the board
printBoard :: [Int] -> IO ()
printBoard board = putStr $ unlines [replicate artifacts '|' | (artifacts, row) <- zip board [1 .. length board]]

boardDivider :: IO ()
boardDivider = putStrLn "\n ---------- Board ------------ \n "

-- Verify if the game have a winner by checking wheter all elements in the board equals to 0
-- meaning that every artifact were removed.
gameFinished :: [Int] -> Bool
gameFinished = all (== 0)

-- Verify whether a plater move is valid or not
checkPlayerMove :: [Int] -> Int -> Int -> Bool
checkPlayerMove board row artifacts = board !! (row - 1) >= artifacts

-- Update the board once a move is made by the player or computer
updateBoard :: [Int] -> Int -> Int -> [Int]
updateBoard board row rowArtifacts = [if rowIndex == row then artifacts - rowArtifacts else artifacts | (artifacts, rowIndex) <- zip board [1 .. length board]]

-- Get the player move via input
getPlayerMove :: String -> IO Int
getPlayerMove move = do
  putStr move
  playerInput <- getChar
  if isDigit playerInput
    then return (digitToInt playerInput)
    else do
      putStrLn "Invalid input, insert a valid number!"
      getPlayerMove move

-- check player dificulty
getDificulty :: String -> IO Int
getDificulty dificulty = do
  putStr dificulty
  putStrLn "Select the dificulty: "
  putStrLn "[1] --> Easy Mode"
  putStrLn "[2] --> Hard Mode"
  playerInput <- getChar
  if isDigit playerInput
    then do
      putStrLn "You selected: "
      print playerInput
      putStrLn "The game is starting, good luck! \n"
      return (digitToInt playerInput)
    else do
      putStrLn "Invalid dificulty, insert a valid dificulty: \n [1] - For Easy Mode \n [2] - For Hard Mode "
      getDificulty dificulty

-- Get wheter is the player turn or the computer
nextTurn :: Int -> Int
nextTurn player = if player == 0 then 1 else 2 -- = computer


-- main function of the game
main :: [Int] -> Int -> IO ()
main board player = 
 do
  getDificulty "----- Starting the game -----"
  do
    boardDivider
    printBoard board
    boardDivider
    if gameFinished board
      then do
        putStr "Player "
        putStr (show (nextTurn player))
        putStrLn " wins!"
      else do
        putStr "Player "
        print player
        row <- getPlayerMove "Enter the row number you want to select: "
        artifacts <- getPlayerMove "Enter the number of artifacts you want to remove: "
        if checkPlayerMove board row artifacts
          then main (updateBoard board row artifacts) (nextTurn player)
          else do
            putStrLn "Invalid move, insert a valid move!"
            main board player

nim :: IO ()
nim = main board 1