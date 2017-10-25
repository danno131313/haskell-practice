module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

data Puzzle =
    Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $
            fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter correctLength aw)
    where correctLength word =
            let l = length (word :: String)
            in  l >= minWordLength
             && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word guessed []
    where guessed = fmap (const Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ ) char =
    char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char =
    char `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar char = case char of
                          Nothing -> '_'
                          Just c  -> c

fillInChar :: Puzzle -> Char -> Puzzle
fillInChar (Puzzle word discovered guessed) char =
    Puzzle word newDiscovered (char : guessed)
        where newDiscovered = 
                zipWith (zipper char) word discovered
              zipper guessChar wordChar discoveredChar =
                if wordChar == guessChar
                then Just wordChar
                else discoveredChar

main :: IO ()
main = putStrLn "hello world"
