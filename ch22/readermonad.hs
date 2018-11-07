-- Functions have a monad instance.

foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+1)

bar :: Foldable t => t -> f a -> (t, Int)
bar r t = (r, length t)

-- we would like to write a single function that combines these two
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

-- or we can combine the functions we already have
