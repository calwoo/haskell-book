import Control.Applicative (liftA2)
import Data.Char

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

m' :: Integer -> Integer
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

hurrDurr :: Integer -> Integer
hurrDurr = do
    a <- hurr
    b <- durr
    return (a + b)

-- exercise:

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
    a <- cap
    b <- rev
    return (a,b)
    
tupledBind :: [Char] -> ([Char], [Char])
tupledBind = cap >>= \a ->
                rev >>= \b ->
                    return (a, b)

-- Reader is a newtype wrapper for the function type.
newtype Reader r a =
    Reader { runReader :: r -> a }
-- here r is the type being "read in", and a is result type.

instance Functor (Reader r) where
    fmap f (Reader ra) =
        Reader $ \r -> f (ra r)

-- exercise: implement this
ask :: Reader a a
ask = Reader id

-- Demonstrating function applicative.
newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address = 
    Address String
    deriving (Eq, Show)

data Person = 
    Person {
        humanName :: HumanName
        , dogName :: DogName
        , address :: Address
    } deriving (Eq, Show)

data Dog =
    Dog {
        dogsName :: DogName
        , dogsAddress :: Address
    } deriving (Eq, Show)

-- to get a dog from a person
--      without Reader:
getDog :: Person -> Dog
getDog p =
    Dog (dogName p) (address p)

--      with Reader:
getDogR :: Person -> Dog
getDogR =
    Dog <$> dogName <*> address

--      with Reader, alternate:
getDogR' :: Person -> Dog
getDogR' =
    liftA2 Dog dogName address

-- exercises: write liftA2 and implement applicative for Reader
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g fa fb = g <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
    pure x = Reader $ \r -> x
    f <*> x = Reader $ \r -> runReader f r $ runReader x r


