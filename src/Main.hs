module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]
type IncorrectGuessCount = Int

data Puzzle =
    Puzzle String [Maybe Char] [Char] IncorrectGuessCount

instance Show Puzzle where
    show (Puzzle _ discovered guessed _) =
        (intersperse ' ' $
            fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
    dict <- readFile "data/words"
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
freshPuzzle word = Puzzle word discovered [] 0
    where discovered = fmap (const Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) char =
    char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) char =
    char `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar char = case char of
                          Nothing -> '_'
                          Just c  -> c

fillInChar :: Puzzle -> Char -> Puzzle
fillInChar (Puzzle word discovered guessed badGuesses) char =
    Puzzle word newDiscovered (char : guessed) badGuesses
        where newDiscovered = 
                zipWith (zipper char) word discovered
              zipper guessChar wordChar discoveredChar =
                if wordChar == guessChar
                then Just wordChar
                else discoveredChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle word discovered guessed badGuesses) guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
          putStrLn $ "You already guessed that character! You have "
                   ++ show (5 - badGuesses) ++ " guesses left!"
          return puzzle
      (True, _) -> do
          putStrLn $ "Good job, you got a character right! You have "
                   ++ show (5 - badGuesses) ++ " guesses left!"
          return (fillInChar puzzle guess)
      (False, _) -> do
          putStrLn $ "Oops, that character isn't in the word! You have " 
                   ++ show (5 - badGuesses) ++ " guesses left!"
          return (fillInChar (Puzzle word discovered guessed (badGuesses + 1)) guess)

gameOver :: Puzzle -> IO()
gameOver (Puzzle word _ _ badGuesses) = 
    if badGuesses > 5 then
        do putStrLn "You lose!"
           putStrLn $ "The word was " ++ word
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO()
gameWin (Puzzle word discovered _ _) =
    if all isJust discovered then
        do putStrLn "You win!"
           putStrLn $ "The word was " ++ word
           exitSuccess
    else return ()

runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "\nCurrent puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "You must input a single character only!"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = 
            freshPuzzle (fmap toLower word)
    runGame puzzle
