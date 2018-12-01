import Data.Char

-- Write a function that retrieves only capital letters in string.
getUpper :: String -> String
getUpper = filter isUpper

-- Write a function that capitalizes the first char in a string.
capFirst :: String -> String
capFirst [] = []
capFirst (x:xs) = (toUpper x):xs

-- Write a function that fully capitalizes a string.
fullCap :: String -> String
fullCap [] = []
fullCap (x:xs) = (toUpper x):(fullCap xs)

-- Write a function that will capitalize the first letter of a string and return only that letter.
capHead :: String -> Char
capHead = head . capFirst