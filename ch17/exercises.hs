newtype Identity a = Identity a deriving Show
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
    pure x = Identity x
    (Identity f) <*> (Identity x) = Identity (f x)

data Pair a = Pair a a deriving Show
instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)
instance Applicative Pair where
    pure x = Pair x x
    (Pair f1 f2) <*> (Pair a1 a2) = Pair (f1 a1) (f2 a2)
