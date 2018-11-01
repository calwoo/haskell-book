-- implement an iterate function with recursion
myIterate :: (a -> a) -> a -> [a]
myIterate f z = (f z) : (myIterate f (f z))