-- instances of semigroups

import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

newtype Identity a = Identity a
instance Semigroup a => Semigroup (Identity a) where
    (Identity a1) <> (Identity a2) = Identity (a1 <> a2)

newtype BoolConj = BoolConj Bool
instance Semigroup BoolConj where
    (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

newtype BoolDisj = BoolDisj Bool
instance Semigroup BoolDisj where
    (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 || b2)

data Or a b = Fst a | Snd b
instance Semigroup a => Semigroup (Or a b) where
    (Fst a1) <> (Fst a2) = Fst (a1 <> a2)
    (Snd b) <> _ = Snd b
    _ <> (Snd b) = Snd b

newtype Combine a b =
    Combine { uncombine :: (a -> b) }
instance Semigroup b => Semigroup (Combine a b) where
    f <> g = Combine $ \a -> (uncombine f) a <> (uncombine g) a

newtype Comp a =
    Comp { unComp :: (a -> a) }
instance Semigroup (Comp a) where
    f <> g = Comp $ unComp f . unComp g
    