newtype State s a =
    State { runState :: s -> (a,s) }

-- in this case, random looks like a state type

-- random :: (Random a) => StdGen -> (a, StdGen)
instance Functor (State s) where
    -- fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $ \s -> let (a,s') = g s
                                     in (f a, s')
                                
instance Applicative (State s) where
    -- pure :: a -> State s a
    pure a = State $ \s -> (a, s)

    (State f) <*> (State g) =
        State $ \s -> let (f', s') = f s
                          (x, s'') = g s'
                      in (f' x, s'')

instance Monad (State s) where
    return = pure
    (State f) >>= g =
        State $ \s -> let (f', s') = f s
                      in runState (g f') s'