{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (nub, sort)

-- The first two consecutive numbers to have two distinct prime factors are:
-- 14 = 2 × 7
-- 15 = 3 × 5
-- The first three consecutive numbers to have three distinct prime factors are:
-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.
-- Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?

data EulerArgs = 
    Euler 
    | AdHoc{ consecutive::Int, num_factors::Int }
    | UnitTest
    deriving (Show, Data, Typeable)

problem0047 :: Int -> Int -> [[Integer]]
problem0047 c l = filter ((==)c . length) $ consecutiveProperty distinctFactors [1..]
    where
        distinctFactors n = l == (unique $ factors n)

consecutiveProperty :: (a -> Bool) -> [a] -> [[a]]
consecutiveProperty test items = worker items []
    where
        worker [] _ = []
        worker (i:is) qs
            | test i = qs' : worker is qs'
            | otherwise = worker is []
            where
                qs' = (if null qs then [] else qs) ++ [i]

consecutivePropertyTest = [
    [[1],[1,1],[1,1,1]] @=? consecutiveProperty ((==)1) [0,0,1,1,1,2,3,4]]

unique :: (Ord a) => [a] -> Int
unique = length . nub . sort

factors :: Integer -> [Integer]
factors 1 = []
factors n = case facts of
    [] -> [n]
    _  -> facts ++ factors (div n $ head facts)
    where facts = take 1 $ filter ((==0) . mod n) [2..(floor $ (fromIntegral n) ** 0.5)]

factorsTest = [
    2 @=? (unique $ factors 14),
    2 @=? (unique $ factors 15),
    3 @=? (unique $ factors 644),
    3 @=? (unique $ factors 645),
    3 @=? (unique $ factors 646),
    [7] @=? factors 7,
    [2,2] @=? factors 4,
    [2] @=? factors 2,
    [] @=? factors 1]

primes :: [Integer]
primes = 2 : [n | n <- [3,5..], isPrime n]

primesTest = [
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] @=? (take 10 primes),
    [2, 3] @=? (take 2 primes),
    2 @=? (head primes)]

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = all noRemainer primes'
    where
        primes' = takeWhile (\p -> p * p <= n) primes
        noRemainer d = 0 /= rem n d

isPrimeTest = [
    assertBool "9 is not prime" (not $ isPrime 9),
    assertBool "6 is not prime" (not $ isPrime 6),
    assertBool "4 is not prime" (not $ isPrime 4),
    assertBool "0 is not prime" (not $ isPrime 0),
    assertBool "3 is prime" (isPrime 3),
    assertBool "2 is prime" (isPrime 2)]    

unitTests = map TestCase $
    isPrimeTest ++
    factorsTest ++
    consecutivePropertyTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let answer = head $ head $ problem0047 4 4
    printf "Answer: %d\n" answer
exec AdHoc{..} = do
    print $ head $ problem0047 consecutive num_factors
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ consecutive = 3, num_factors = 4 },
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
