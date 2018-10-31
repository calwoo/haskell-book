thirdLetter :: String -> Char
thirdLetter x = x !! 2

curryString :: String
curryString = "Curry is awesome!"

letterIndex :: Int -> Char
letterIndex x = curryString !! x