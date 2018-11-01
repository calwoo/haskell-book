data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x = if x < 0 then Nothing else
    let (Just n) = integerToNat (x - 1) in
        Just (Succ n)