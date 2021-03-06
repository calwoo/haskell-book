instance (Monoid a, Monoid b) => Monoid (a,b) where
    mempty = (mempty,mempty)
    (a,b) `mappend` (a',b') =
        (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) =
        (u `mappend` v, f x)

