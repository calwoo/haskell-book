class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b d
    first f = bimap f id

    second :: (c -> d) -> p a c -> p b d
    second = bimap id

