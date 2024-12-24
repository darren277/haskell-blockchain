module Main where

import Blockchain (main)  -- Adjust this to match the module name and function

main :: IO ()
main = do
    putStrLn "Initializing Blockchain App..."
    Blockchain.main  -- Calls the `main` function from `Blockchain`
