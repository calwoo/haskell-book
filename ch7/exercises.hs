tensDigit :: Integral a => a -> a
tensDigit x = snd (divMod x 10)

hunsD x = snd (divMod x 100)

foldBool :: a -> a -> Bool -> a
foldBool x y t
    | t = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x,y) = (f x, y)

