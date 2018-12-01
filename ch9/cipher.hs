module Cipher where

import Data.Char

-- chr :: Int -> Char sends int to unicode
-- ord :: Char -> Int sends char to unicode encoding

caesar :: Int -> String -> String
caesar n = map (shift n)

shift :: Int -> (Char -> Char)
shift n = \c -> chr (((n + (ord c)) `mod` 26) + (ord 'a'))

unCaesar :: Int -> String -> String
unCaesar n = map (shift (-n))