{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (find)
import Data.Maybe (isJust, catMaybes)

-- It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
-- 9 = 7 + 2×1^2
-- 15 = 7 + 2×2^2
-- 21 = 3 + 2×3^2
-- 25 = 7 + 2×3^2
-- 27 = 19 + 2×2^2
-- 33 = 31 + 2×1^2
-- It turns out that the conjecture was false.
-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

data EulerArgs = 
    Euler 
    | Conjecture{ range::Integer }
    | AdHoc{ limit::Int }
    | UnitTest
    deriving (Show, Data, Typeable)

problem0046 :: [Integer]
problem0046 = filter (\n -> not (isPrime n || (isJust $ goldbach n))) [3,5..]

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

goldbach :: Integer -> Maybe (Integer, Integer)
goldbach n = find (uncurry conjecture) [(p, sqrt (div (n - p) 2)) | 
    p <- (takeWhile (<n) primes)]
    where
        conjecture p s = n == p + 2 * s ^ 2
        sqrt s = floor ((fromIntegral s) ** 0.5)

goldbachTest = [
    Nothing @=? goldbach 3,
    Nothing @=? goldbach 17,
    Just (3, 4) @=? goldbach 35,
    Just (7, 3) @=? goldbach 25,
    Just (19, 2) @=? goldbach 27,
    Just (31, 1) @=? goldbach 33]

unitTests = map TestCase $
    isPrimeTest ++
    goldbachTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let answer = head problem0046
    printf "Answer: %d\n" answer
exec AdHoc{..} = do
    mapM_ print $ take limit $ problem0046
exec Conjecture{..} = do
    mapM_ (\(p, s) -> printf "%3d = %3d + 2 * %3d^2\n" (p + 2*s^2) p s) $
        catMaybes [goldbach n | n <- [1,3..range]]
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ limit = 4 },
        Conjecture{ range = 30 },
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
