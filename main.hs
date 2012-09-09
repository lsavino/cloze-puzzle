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
    putStrLn $ "Removed words: " ++ (concat $ intersperse " " (Data.List.sort (map word removedWords)))


blankWords :: ([String], [RemovedWord]) -> Int -> String -> ([String], [RemovedWord])
blankWords (remainingWords, removedWords) i word =
    if (rem i 5) == 0
    then (remainingWords ++ ["__" ++ show (div i 5) ++ "__"], removedWords ++ [RemovedWord word (div i 5)])
    else (remainingWords ++ [word], removedWords)

data RemovedWord = RemovedWord {
    word :: String,
    index :: Int
}
