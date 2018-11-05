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

