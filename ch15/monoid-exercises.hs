import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    Trivial <> Trivial = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

newtype Identity a = Identity a deriving Show
instance Semigroup a => Semigroup (Identity a) where
    (Identity a1) <> (Identity a2) = Identity (a1 <> a2)
instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity a1) (Identity a2) = Identity (mappend a1 a2)

newtype BoolConj = BoolConj Bool
instance Semigroup BoolConj where
    (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)
instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

newtype BoolDisj = BoolDisj Bool
instance Semigroup BoolDisj where
    (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 || b2)
instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

newtype Combine a b =
    Combine { uncombine :: (a -> b) }
instance Semigroup b => Semigroup (Combine a b) where
    f <> g = Combine $ \a -> (uncombine f) a <> (uncombine g) a
instance Monoid b => Monoid (Combine a b) where
    mempty = Combine $ \a -> mempty
    mappend f g = Combine $ \a -> mappend ((uncombine f) a) ((uncombine g) a)

newtype Comp a =
    Comp { unComp :: (a -> a) }
instance Semigroup (Comp a) where
    f <> g = Comp $ unComp f . unComp g
instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }
instance Monoid a => Semigroup (Mem s a) where
    f <> g = mappend f g
instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend f g =
        let f' = runMem f
            g' = runMem g in
                Mem $ \s -> let (a,s') = f' s
                                (a',s'') = g' s' in
                                    (mappend a a', s'')

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0
