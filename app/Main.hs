-- Alunos: Lucas Piazzi de Castro (201635003) e Cristiano Nascimento (201635029) 

module Main where
import Data.Char
import Control.Monad.Random
import System.IO.Unsafe
import Foreign.C.Types

main :: IO ()
main = do
    selectedDifficulty <- getDifficulty 
    nim board (if selectedDifficulty == 1 then 1 else 0) selectedDifficulty

-- nim function of the game
nim :: [Int] -> Int -> Int -> IO ()
nim board player difficulty = do
    printBoard board
    if gameFinished board
      then do
        putStr "Player: "
        putStr (show (nextTurn player))
        putStrLn " wins!"
    else do
        putStr "Player "
        print player
        putStr " turn! \n"
        -- player turn
        if player == 1 then do
          row <- getPlayerMove "Enter the row number you want to select:  "
          putStr "\n"
          artifacts <- getPlayerMove "Enter the number of artifacts you want to remove: "
          putStr "Move made! \n"
          if checkPlayerMove board row artifacts then 
            nim (updateBoard board row artifacts) (nextTurn player) (difficulty)
          else do
            putStrLn "Invalid move, insert a valid move!"
            nim board player difficulty
        -- computer turn
        else do
          -- head is the selected row to remove, and the tail is how many artifacts will be removed
          let rowAndArtifacts = getComputerMove board difficulty
          putStrLn "\n The computer made his move"
          nim (updateBoard board (head rowAndArtifacts) (last rowAndArtifacts)) (nextTurn player) (difficulty)


-- each index represents a line of the board, the value in each
-- index represents the number of artifacts in the current board.
board :: [Int]
board = [1, 3, 5, 7]

-- print the current state of the board, number of artifacts in each row
printBoard :: [Int] -> IO ()
printBoard board = do
  boardDivider
  putStr $ unlines ["[" ++ (show row) ++ "] " ++ replicate artifacts '|' | (artifacts, row) <- zip board [1 .. length board]]
  boardDivider

boardDivider :: IO ()
boardDivider = putStrLn "\n ---------- Board ------------ \n "

-- Update the board once a move is made by the player or computer
updateBoard :: [Int] -> Int -> Int -> [Int] -- returns a updated board
updateBoard board row rowArtifacts = [if rowIndex == row then artifacts - rowArtifacts else artifacts | (artifacts, rowIndex) <- zip board [1 .. length board]]

-- workaround for the getChar bug
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

-- Get the player move via input
getPlayerMove :: String -> IO Int
getPlayerMove move = do
  putStr move
  playerInput <- getHiddenChar
  if isDigit playerInput
    then return (digitToInt playerInput)
  else do
    putStrLn "Invalid input, insert a valid number! \n"
    getPlayerMove move

-- Get the computer move based on the difficulty selected
getComputerMove :: [Int] -> Int -> [Int]
getComputerMove board difficulty = do
  if difficulty == 1 then makeRandomMove board  else makeRandomMove board

makeRandomMove :: [Int] -> [Int]
makeRandomMove board = do 
      let randomSelectedRow = unsafePerformIO (getStdRandom (randomR (0, 3))) :: Int
      -- if the row is empty, call the function again
      if (board !! randomSelectedRow) == 0 then 
        makeRandomMove board
      else do 
        let randowSelectedArtifacts = unsafePerformIO (getStdRandom (randomR(1, board !! randomSelectedRow))) :: Int 
        randomSelectedRow : randowSelectedArtifacts : []

-- Return optimal move for the hard mode
--makeOptimalMove ::[Int] -> [Int]

-- Verify whether a plater move is valid or not
checkPlayerMove :: [Int] -> Int -> Int -> Bool
checkPlayerMove board row artifacts = board !! (row - 1) >= artifacts

-- Verify if the game have a winner by checking wheter all elements in the board equals to 0
-- meaning that every artifact were removed.
gameFinished :: [Int] -> Bool
gameFinished board = 
   if all (== 0) board then True else False

-- check player difficulty
getDifficulty :: IO Int
getDifficulty  = do
  putStrLn "----- Starting the game -----"
  putStrLn "Select the difficulty: "
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
      getDifficulty 

-- Get wheter is the player turn or the computer
-- player = 0 --> computer turn
-- player = 1 --> player turn
nextTurn :: Int -> Int
nextTurn player = if player == 0 then 1 else 0

