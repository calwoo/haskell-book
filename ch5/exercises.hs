i :: a -> a
i = id

c :: a -> b -> a
c x _ = x

c'' :: b -> a -> b
c'' x _ = x

c' :: a -> b -> b
c' _ x = x

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f x = f x

data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, f w)

f' :: Int -> String
f' = undefined

g' :: String -> Char
g' = undefined

h :: Int -> Char
h = g' . f'

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge m n = fst . n . m