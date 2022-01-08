main :: IO()
main = do
       putStrLn "----- Starting the game -----"
       putStrLn "Select the dificulty: "
       putStrLn "[1] --> Easy Mode"
       putStrLn "[2] --> Hard Mode"
       dificulty <- getLine
       putStrLn "You selected: "
       print (dificulty)
       putStrLn "Good Luck!"
