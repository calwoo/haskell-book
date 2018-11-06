-- Write monad instances for the following.
-- 1)
data Nope a =
    NopeDotJpg
instance Monad Nope where
    NopeDotJpg >>= _ = NopeDotJpg
    return _ = NopeDotJpg
instance Applicative Nope where
    pure = return
    ff <*> fx = 
        do
            f <- ff
            x <- fx
            return (f x)
instance Functor Nope where
    fmap f xs = xs >>= return . f

-- 2)
data PhhhbbtttEither a b =
    PLeft a
    | PRight b
instance Monad (PhhhbbtttEither a) where
    return b = PRight b
    PLeft a >>= _ = PLeft a 
    PRight b >>= f = f b
instance Applicative (PhhhbbtttEither a) where
    pure = return
    ff <*> fx = do
        f <- ff
        x <- fx
        return (f x)
instance Functor (PhhhbbtttEither a) where
    fmap f xs = xs >>= return . f

-- 3)
newtype Identity a = Identity a 
    deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)
instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x

-- 4)
data List a =
    Nil
    | Cons a (List a)
instance Monad List where
    return x = Cons x Nil
    Nil >>= _ = Nil
    (Cons x xs) >>= f = (f x) `appendList` (xs >>= f)
instance Applicative List where
    pure = return
    ff <*> fx = do
        f <- ff
        x <- fx
        return (f x)
instance Functor List where
    fmap f xs = xs >>= return . f

appendList :: List a -> List a -> List a 
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x (appendList xs ys)

-- Write the following functions using the methods in monad/functor.
-- 1)
j :: Monad m => m (m a) -> m a 
j mma = mma >>= id 

-- 2)
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= return . f

-- 3)
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
    a <- ma
    b <- mb
    return (f a b)

-- 4)
a :: Monad m => m a -> m (a -> b) -> m b
a  ma mf = mf >>= \f -> ma >>= return . f

-- 5)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (a:as) f = do
    b <- f a 
    bs <- meh as f
    return $ b:bs

-- 6)
flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id