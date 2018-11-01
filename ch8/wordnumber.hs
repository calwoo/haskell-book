module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
    case n of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"
        _ -> error "not a digit"

digits :: Int -> [Int]
digits n
    | n == 0 = []
    | otherwise = (digits (n `div` 10)) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits