module Main where
import Data.Char
import Lib

-- each index represents a line of the board, the value in each
-- index represents the number of artifacts in the current board.
board :: [Int]
board = [1,3,5,7]

-- print the current state of the board 
printBoard :: [Int] -> IO ()
printBoard board = putStr $ unlines [replicate stars '|'| (stars, row) <- zip board [1..length board]]

boardDivider :: IO()
boardDivider = putStrLn "\n ---------- Board ------------ \n "

main = do
       print "----- Starting the game -----"
       putStrLn "Select the dificulty: "
       putStrLn "[1] --> Easy Mode"
       putStrLn "[2] --> Hard Mode"
       dificulty <- getLine  
       putStrLn "You selected: "
       print dificulty
       putStrLn "Good Luck!"
       boardDivider
       printBoard board
       boardDivider

