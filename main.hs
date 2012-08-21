module Main where

import Data.String
import System.Environment
import System.IO

main :: IO ()
main = do
    (filename:args) <- getArgs
    fullText  <- (readFile filename)
    putStrLn fullText
