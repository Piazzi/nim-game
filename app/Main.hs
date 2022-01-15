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
            putStrLn "\n Invalid move, insert a valid move!"
            nim board player difficulty
        -- computer turn
        else do
          -- head is the selected row to remove, and the tail is how many artifacts will be removed
          let rowAndArtifacts = getComputerMove board difficulty
          putStrLn "\n The computer made his move the following move: "
          putStr "Selected Row: "
          print (head rowAndArtifacts)
          putStr "Selected Artifacts: "
          print (last rowAndArtifacts)
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

-- Verify whether a player move is valid or not
checkPlayerMove :: [Int] -> Int -> Int -> Bool
checkPlayerMove board row artifacts = board !! (row - 1) >= artifacts

-- Verify if the game have a winner by checking wheter all elements in the board equals to 0
-- meaning that every artifact were removed.
gameFinished :: [Int] -> Bool
gameFinished board = 
   if all (<= 0) board then True else False

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

makeRandomMove :: [Int] -> [Int]
makeRandomMove board = do 
      let randomSelectedRow = unsafePerformIO (getStdRandom (randomR (0, 3))) :: Int
      let totalArtifactsInTheRow =  board !! randomSelectedRow
      -- if the row is empty, call the function again
      if (totalArtifactsInTheRow <= 0) then
         makeRandomMove board
      else do 
        let randowSelectedArtifacts = unsafePerformIO (getStdRandom (randomR(1, totalArtifactsInTheRow))) :: Int 
        if(randowSelectedArtifacts >= totalArtifactsInTheRow) then
            makeRandomMove board
        else do
        [randomSelectedRow, randowSelectedArtifacts]
 
-- Return optimal move for the hard mode
makeOptimalMove ::[Int] -> [Int]
makeOptimalMove board = makeOptimalMoveAux board 0 (head board)

makeOptimalMoveAux :: [Int] -> Int -> Int -> [Int]
makeOptimalMoveAux board row artifactsToRemove = 
    -- make the random move if necessary
    if ((row == (length board)) && (artifactsToRemove == 0)) then do
        (makeRandomMove board) -- easy move
    else if (artifactsToRemove == 0) then
        makeOptimalMoveAux board (row + 1) (board !! row)
    else if (isEvenList (sumBinaryNumbers (removeArtifactsFromRow board row artifactsToRemove))) then do
        return [] (row, artifactsToRemove) 
    else
        makeOptimalMoveAux board row (artifactsToRemove - 1)

-- Removes artifacts from a given row
removeArtifactsFromRow :: [Int] -> Int -> Int -> [Int]
removeArtifactsFromRow board chosenRow removedArtifacts = 
    [if row == chosenRow then artifacts - removedArtifacts else artifacts | (artifacts, row) <- zip board [1..length board]]

-- Returns a list that contains the sum of the numbers in binary
sumBinaryNumbers :: [Int] -> [Int]
sumBinaryNumbers [] = []
sumBinaryNumbers (x : xs) = sumBinaryRows (concat [take (3 - (length (intToBin x))) (repeat 0), (intToBin x)]) (sumBinaryNumbers xs)

-- Returns a list which each index is the sum of the binary numbers in each row
sumBinaryRows :: [Int] -> [Int] -> [Int]
sumBinaryRows [] [] = []
sumBinaryRows (x:xs) [] = (x:xs)
sumBinaryRows [] (x:xs) = (x:xs)
sumBinaryRows (x:xs) (y:ys) = x + y : sumBinaryRows xs ys

-- Convert a int to binary
intToBin :: Int -> [Int]
intToBin 0 = [0]
intToBin n = reverse (toBinHelper n)

-- Helper function to convert a number to binary
toBinHelper :: Int -> [Int]
toBinHelper 0 = []
toBinHelper n = let (q,r) = n `divMod` 2 in r : toBinHelper q

-- Checks if all the elements in a list are even
isEvenList :: [Int] -> Bool
isEvenList [] = True
isEvenList (x:xs) =  if ((x `mod` 2) /= 0) then False else True