-- Write functor instances for the following datatypes.

data Quant a b =
    Finance
    | Desk a
    | Floor b
instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Floor n) = Floor (f n)

data K a b =
    K a
instance Functor (K a) where
    fmap _ (K a) = K a

data EvilGoateeConst a b =
    GoatyConst b
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst a) = GoatyConst (f a)

data LiftItOut f a =
    LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

data Parappa f g a =
    DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap k (DaWrappa fa ga) = DaWrappa (fmap k fa) (fmap k ga)

data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
    fmap k (IgnoringSomething fa gb) = IgnoringSomething fa (fmap k gb)

data Notorious g o a t =
    Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)

data List a =
    Nil
    | Cons a (List a)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a1 a2 a3) =
        MoreGoats (fmap f a1) (fmap f a2) (fmap f a3)

data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read m) = Read (f . m)
