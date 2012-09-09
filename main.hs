module Main where

import Control.Monad(when)
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
    putStrLn $ "\n\nRemoved words: " ++ (concat $ intersperse " " (Data.List.sort removedWords))

    askForGuess removedWords

blankWords :: ([String], [String]) -> Int -> String -> ([String], [String])
blankWords (remainingWords, removedWords) i word =
    if (rem i 5) == 0
    then (remainingWords ++ ["__" ++ show (div i 5) ++ "__"], removedWords ++ [word]) 
    else (remainingWords ++ [word], removedWords)

data RemovedWord = RemovedWord {
    word :: String,
    index :: Int
}

askForGuess :: [String] -> IO ()
askForGuess removedWords = do
    putStrLn "\nEnter a guess:"
    theGuess <- getLine
    when (not $ Data.List.null theGuess) $ do
        let (indexString:tail) = (words theGuess)
            index = read indexString
            guess = head tail
            isValidGuess = checkGuess removedWords (index, guess)
        putStrLn $ "\n\nYou said: " ++ guess ++ "\nValid? " ++ show isValidGuess 
        askForGuess removedWords

checkGuess :: [String] -> (Int, String) -> Bool
checkGuess removedWords (index, guess) =
    elem guess removedWords
