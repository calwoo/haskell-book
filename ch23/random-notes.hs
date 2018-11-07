import System.Random

-- we can use mkStdGen :: Integer -> StdGen to make a random generator using a seed.

sg = mkStdGen 0

-- using next, we can get a value: next sg.
-- this is pure, so we get the same values each time we plug in a single value.
-- However, we also get a new StdGen back

newSg = snd (next sg)
newSg2 = snd (next (snd (next sg)))

-- and so on...

-- other functions:
-- random gives other values.
-- randomR gives values in a range.

-- We can relieve ourselves of this tedium by using the State monad.