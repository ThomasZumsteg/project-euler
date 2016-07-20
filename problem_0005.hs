{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

import Text.Printf
import System.Console.CmdArgs
import Data.Time

data EulerArgs = 
    Limits { 
        upper :: Integer,
        lower :: Integer } 
    | Factors { 
        factors :: [Integer] }
    | Euler 
        deriving (Show, Data, Typeable)
    
limits = Limits{ upper = 10, lower = 1 }
factors_ = Factors{ factors = [1..10] }
euler = Euler{}

problem0005 :: (Integral a) => [a] -> a 
problem0005 factors = product commonFactors
    where
        commonFactors = foldl1 combineLists factorList
        factorList = map (getFactors 2) factors

getFactors :: (Integral a) => a -> a -> [a]
getFactors _ 1 = [1]
getFactors f n
    | n < f * f = [n]
    | 0 == mod n f = f : getFactors f (quot n f)
    | otherwise = getFactors (succ f) n

combineLists :: (Integral a) => [a] -> [a] -> [a]
combineLists [] ys = ys
combineLists xs [] = xs
combineLists xs@(x:xs')  ys@(y:ys')
    | x < y = x : combineLists xs' ys
    | x > y = y : combineLists xs ys'
    | otherwise = x : combineLists xs' ys'

exec :: EulerArgs -> Integer
exec Limits{..} = problem0005 [lower..upper]
exec Factors{..} = problem0005 factors
exec Euler = problem0005 [1..20]

main :: IO ()
main = do
    args <- cmdArgs $ modes [limits, factors_, euler]
    start <- getCurrentTime
    print $ exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start

