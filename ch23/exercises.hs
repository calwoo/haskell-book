-- Write the functions:

newtype State s a =
    State { runState :: s -> (a,s) }

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)




