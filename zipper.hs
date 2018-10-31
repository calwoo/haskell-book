module Zipper where

-- Suppose we wanted to build a maze. We decorate each part of the maze with an element.

data Node a = DeadEnd a
    | Passage a (Node a)
    | Fork a (Node a) (Node a)

-- Helper functions to get value/put value.

get :: Node a -> a
get (DeadEnd x) = x
get (Passage x _) = x
get (Fork x _ _) = x

put :: Node a -> a -> Node a
put (DeadEnd _) x = DeadEnd x
put (Passage _ n) x = Passage x n
put (Fork _ n1 n2) x = Fork x n1 n2

-- Let's put a test maze we can play with. We will decorate nodes with coordinates.