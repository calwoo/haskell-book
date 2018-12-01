stops = "pbtdkg"
vowels = "aeiou"

aFunction :: [a] -> [a] -> [(a,a,a)]
aFunction stops vowels = [(s1,v,s2) | s1 <- stops, v <- vowels, s2 <- stops]

-- Rewrite functions using folds.

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\a b -> (p a) || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> (f a) : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\a b -> if p a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id