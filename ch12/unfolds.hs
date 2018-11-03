-- implement an iterate function with recursion
myIterate :: (a -> a) -> a -> [a]
myIterate f z = (f z) : (myIterate f (f z))

-- implement unfold using recursion
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = case f z of
                    Nothing -> []
                    Just (a, b) -> a : (myUnfoldr f b)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myUnfoldr (\a -> Just (f a, f a)) z

-- binary trees
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show, Ord)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f z = case f z of
                Nothing -> Leaf
                Just (a1,b,a2) -> Node (unfold f a1) b (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\i -> if i < n then Just (i+1,i,i+1)
                                     else Nothing) 0
