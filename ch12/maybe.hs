

-- simple boolean check for maybe values
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

-- maybe catamorphism
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

-- fallback
fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- convert from list to maybe
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- drop nothings from list
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\a b -> case a of
                            Nothing -> b
                            Just x -> x : b) []

-- sequence?
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (\a b -> case a of
                            Nothing -> Nothing
                            Just x -> case b of
                                        Nothing -> Nothing
                                        Just l -> Just (x : l)) (Just [])