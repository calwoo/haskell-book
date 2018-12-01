myOr :: [Bool] -> Bool
myOr bs = length (filter id bs) > 0

myAny :: (a -> Bool) -> [a] -> Bool
myAny p as = length (filter p as) > 0

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

