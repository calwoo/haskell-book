-- Implement the following functions in terms of foldMap and foldr from
-- the Foldable typeclass.

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\a b -> (x == a) || b) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a 
minimum = foldr (\a b -> case b of
                            Nothing -> Just a
                            Just n -> if n < a
                                        then Just n
                                        else Just a) Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (\a b -> case b of
                            Nothing -> Just a
                            Just n -> if n > a
                                        then Just n
                                        else Just a) Nothing

null :: (Foldable t) => t a -> Bool
null = foldr (\a b -> True) False

length :: (Foldable t) => t a -> Int
length = foldr (\a b -> 1 + b) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> (f a) `mappend` b) mempty


-- Write Foldable instances for the following datatypes.
data Constant a b =
    Constant a
instance Foldable (Constant a) where
    foldr _ z (Constant a) = z
    
data Two a b =
    Two a b
instance Foldable (Two a) where
    foldr f z (Two a b) = f b z

data Three a b c =
    Three a b c
instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z

data Three' a b =
    Three' a b b
instance Foldable (Three' a) where
    foldr f z (Three' a b1 b2) = f b1 (f b2 z)

data Four' a b =
    Four' a b b b
instance Foldable (Four' a) where
    foldr f z (Four' a b1 b2 b3) = f b1 (f b2 (f b3 z))

-- Write a filter function for Foldables using foldMap
filterM :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterM p fa = foldMap (\a -> if p a
                                then pure a
                                else mempty) fa
