module RandomExample2 where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die =
    DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Integer -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "intToDie got non 1-6 integer" ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1,6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
    intToDie <$> state (randomR (1,6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
    liftA3 (,,) rollDie rollDie rollDie

-- repeat :: a -> [a] repeats an input, obviously
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- wrong function! we just repeated the die value, we didn't actually run the die
-- infinitely many times.

-- To do this, we need a monadic binding.
-- (use replicateM :: Monad m => Int -> m a -> m [a])

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- How about rolling die until we get 20 or higher?
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
            | sum >= 20 = count
            | otherwise = 
                let (die, nextGen) = randomR (1,6) gen
                in go (sum+die) (count+1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
            | sum >= n = count
            | otherwise =
                let (die, nextGen) = randomR (1,6) gen
                in go (sum+die) (count+1) nextGen

rollsCountLogged :: Integer -> StdGen -> (Integer, [Die])
rollsCountLogged n = go 0 0 []
    where go :: Integer -> Integer -> [Die] -> StdGen -> (Integer, [Die])
          go sum count log gen
            | sum >= n = (count, log)
            | otherwise =
                let (die, nextGen) = randomR (1,6) gen
                in go (sum+die) (count+1) (intToDie die : log) nextGen