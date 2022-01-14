-- Alunos: Lucas Piazzi de Castro (201635003) e Cristiano Nascimento (201635029) 

module Main where

import Lib
import Data.Char
import Control.Monad.Random

nimGame :: IO ()
nimGame = do
    -- transform IO Int to Int
    selectedDifficulty <- getdifficulty
    if (selectedDifficulty == 1) then
          main board 1 selectedDifficulty
      else
          main board 0 selectedDifficulty

-- main function of the game
main :: [Int] -> Int -> Int -> IO ()
main board player difficulty = do
    boardDivider
    printBoard board
    boardDivider
    if gameFinished board
      then do
        putStr "Player: "
        putStr (show (nextTurn player))
        putStrLn " wins!"
    else do
        putStr "Player "
        print player
        print " turn!"
        -- player turn
        if player == 1 then do
          row <- getPlayerMove "Enter the row number you want to select: "
          artifacts <- getPlayerMove "Enter the number of artifacts you want to remove: "
          putStr "Move made!"
          if checkPlayerMove board row artifacts
            then main (updateBoard board row artifacts) (nextTurn player) (difficulty)
          else do
            putStrLn "Invalid move, insert a valid move!"
            main board player difficulty
        -- computer turn
        else do
          -- ead is the selected row to remove, and the tail is how many artifacts will be removed
          let rowAndArtifacts = getComputerMove board difficulty
          main (updateBoard board (head rowAndArtifacts) (last rowAndArtifacts)) (nextTurn player) (difficulty)


-- each index represents a line of the board, the value in each
-- index represents the number of artifacts in the current board.
board :: [Int]
board = [1, 3, 5, 7]

-- print the current state of the board, number of artifacts in each row
printBoard :: [Int] -> IO ()
printBoard board = putStr $ unlines [replicate artifacts '|' | (artifacts, row) <- zip board [1 .. length board]]

boardDivider :: IO ()
boardDivider = putStrLn "\n ---------- Board ------------ \n "

-- Update the board once a move is made by the player or computer
updateBoard :: [Int] -> Int -> Int -> [Int] -- returns a updated board
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

-- Get the computer move based on the difficulty selected
getComputerMove :: [Int] -> Int -> [Int]
getComputerMove board difficulty = do
  if difficulty == 1 then makeRandomMove board  else makeRandomMove board

 -- Return a random move for the easy mode
makeRandomMove :: [Int] -> [Int]
makeRandomMove board = do
  let randomSelectedRow = getRandomR (0, 3)
  -- if the row is empty, call choose again
  if (board!!randomSelectedRow) == 0 then do
    makeRandomMove board
  else do
    let randowSelectedArtifacts = getRandomR (1, board!!randomSelectedRow)
    return [randomSelectedRow, randowSelectedArtifacts]

-- Return optimal move for the hard mode
--makeOptimalMove ::[Int] -> [Int]


-- Verify whether a plater move is valid or not
checkPlayerMove :: [Int] -> Int -> Int -> Bool
checkPlayerMove board row artifacts = board !! (row - 1) >= artifacts

-- Verify if the game have a winner by checking wheter all elements in the board equals to 0
-- meaning that every artifact were removed.
gameFinished :: [Int] -> Bool
gameFinished = all (== 0)

-- check player difficulty
getdifficulty :: IO Int
getdifficulty  = do
  putStrLn "----- Starting the game -----"
  putStrLn "Select the difficulty: "
  putStrLn "[1] --> Easy Mode"
  putStrLn "[2] --> Hard Mode"
  playerInput <- getDigit
  if (playerInput == 0 || playerInput == 1)
    then do
      putStrLn "You selected: "
      print playerInput
      putStrLn "The game is starting, good luck! \n"
      return playerInput
    else do
      putStrLn "Invalid difficulty, insert a valid difficulty: \n [1] - For Easy Mode \n [2] - For Hard Mode "
      getdifficulty

-- Get wheter is the player turn or the computer
-- player = 0 --> computer turn
-- player = 1 --> player turn
nextTurn :: Int -> Int
nextTurn player = if player == 0 then 1 else 0

