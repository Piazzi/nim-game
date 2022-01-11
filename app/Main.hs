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

-- Verify if the game have a winner by checking wheter all elements in a list equals to 0
-- meaning that every artifact were removed.
gameFinished :: [Int] -> Bool
gameFinished = all (== 0)

-- Verify whether a plater move is valid or not
checkPlayerMove :: [Int] -> Int -> Int -> Bool
checkPlayerMove board row artifacts = (board !! (row - 1)) >= artifacts

-- Update the board once a move is made by the player or computer
updateBoard :: [Int] -> Int -> Int -> [Int]
updateBoard board row rowArtifacts = [if rowIndex == row then artifacts - rowArtifacts else artifacts | (artifacts, rowIndex) <- zip board [1 .. length board]]

-- Get the player move via input
getPlayerMove :: String -> IO Int
getPlayerMove move = do putStr move
                        playerInput <- getChar
                        if isDigit playerInput then
                         do return (digitToInt playerInput)
                        else 
                         do putStrLn "Invalid input, insert a valid number!"
                            getPlayerMove move


-- main function of the game
main = do
  print "----- Starting the game -----"
  putStrLn "Select the dificulty: "
  putStrLn "[1] --> Easy Mode"
  putStrLn "[2] --> Hard Mode"
  dificulty <- getLine
  putStrLn "You selected: "
  print dificulty
  putStrLn "The game is starting, good luck! \n"
  boardDivider
  printBoard board
  boardDivider
  do  
      if gameFinished board
        then do
          putStr "Player "
          putStr (show (next player))
          putStrLn " wins!"
        else do
          putStr "Player "
          putStrLn (show player)
          row <- getDigit "Enter row number: "
          stars <- getDigit "Enter stars to remove: "
          if checkValidMove board row stars
            then playnim (generateNewBoard board row stars) (next player)
            else do
              putStrLn "Invalid move"
              playnim board player
