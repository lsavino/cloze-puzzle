module Main where

import Data.List
import Data.Sequence
import Data.String
import System.Environment
import System.IO

main :: IO ()
main = do
    (filename:args) <- getArgs
    fullText  <- (readFile filename)
    let sequencedWords = fromList $ words fullText
        (blankedText, removedWords) = foldlWithIndex blankWords ([], []) sequencedWords
        displayText = concat $ intersperse " " blankedText
    putStrLn displayText
    putStrLn $ "Removed words: " ++ (concat $ intersperse " " removedWords)


blankWords :: ([String], [String]) -> Int -> String -> ([String], [String])
blankWords (remainingWords, removedWords) i word =
    if (rem i 5) == 0
    then (remainingWords ++ ["_____"], removedWords ++ [word])
    else (remainingWords ++ [word], removedWords)
