import Control.Applicative

-- datatype that corresponds to identity
newtype Identity a =
    Identity { runIdentity :: a }

-- datatype that corresponds to function composition
newtype Compose f g a = 
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

-- lets start by composing functors
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga

-- we can generalize this
newtype One f a =
    One (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

-- this is an incredibly perverse way to show that composition of functors are functors
-- we could have done this so much easier categorically, but okay.
-- computer scientists be computer scientists.

-- now, applicative functors (ie, lax monoidal functors) also compose
instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
    -- pure :: a -> Compose f g a
    pure x = Compose $ (pure . pure) x

    -- (<*>) :: Compose f g (a -> b) ->
    --            Compose f g a -> Compose f g b
    (Compose fgfun) <*> (Compose fga) =
        Compose $ (liftA2 . liftA2) ($) fgfun fga

-- fun fun!

-- however, the crux of all this is that monads do NOT compose.
