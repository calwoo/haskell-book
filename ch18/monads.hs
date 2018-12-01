-- Monads are functors and applicatives, ie, they are monoidal functors.
-- (Strictly, they are lax monoidal functors on categories with tensorial strength)

-- fmap f xs = xs >>= return . f
-- this is actually a compatibility law with the functorial instance.

-- Functor => Applicative => Monad is the typeclass dependencies.

-- Monads are given by 2 main ops:
--      (>>=) :: m a -> (a -> m b) -> m b
--      return :: a -> m a
-- we can derive from it
--      (>>) :: m a -> m b -> m b
-- which is >>= except that we forget the first computation's result and move on to the 2nd.

-- Really, monadically, everything comes from join :: m (m a) -> m a
-- we can get bind from this.

join :: Monad m => m (m a) -> m a
join mma = mma >>= id

bind :: Monad m => m a -> (a -> m b) -> m b
bind ma f = join $ fmap f ma

-- some examples of monads in action:
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]

data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n 
    | n >= 0 = Just n
    | otherwise = Nothing

    -- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of 
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just weighty ->
                            weightCheck (Cow nammy agey weighty)

-- we can clean this up using do-notation.
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    namey <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow namey agey weighty)

-- in terms of nested lambdas, this is
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>=
        \namey ->
        noNegative age' >>=
            \agey ->
            noNegative weight' >>=
                \weighty ->
                weightCheck (Cow namey agey weighty)

