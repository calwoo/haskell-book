import Data.Char

-- Function returns True when all values in first list appear in second list.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf l@(x:xs) l2 = (elem x l2) && (isSubsequenceOf xs l2)

-- Split sentence into words, then tuple each word with capitalized form.
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map capper (words s)

capper :: String -> (String, String)
capper s@(c:cs) = (s, (toUpper c):cs)

