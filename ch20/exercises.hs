-- Implement the following functions in terms of foldMap and foldr from
-- the Foldable typeclass.

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\a b -> (x == a) || b) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a 
