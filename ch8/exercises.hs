arithSum :: (Eq a, Num a) => a -> a
arithSum n
    | n == 0 = 0
    | otherwise = n + arithSum (n - 1)

naiveMult :: (Integral a) => a -> a -> a
naiveMult x y = multAcc x y 0
    where multAcc x y acc
            | y == 0 = acc
            | otherwise = multAcc x (y-1) (acc+x)

mc91 :: Integer -> Integer
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 (mc91 (x + 11))