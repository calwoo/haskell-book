import Data.List (intersperse)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe ss = concat (intersperse " " (map (\s -> if s == "the" then "a" else s) (words ss)))

