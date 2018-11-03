module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (RandomRIO)


main :: IO ()
main = putStrLn "Hello, Haskell!"

type WordList = [String]

allWords :: IO WordList
allWord = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 10

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
        let l = length (w :: String)
        in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, (length wl) - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle key =
    Puzzle key (map (const Nothing) key) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guesses) c = elem c guesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

