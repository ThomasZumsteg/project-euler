{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.
-- Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs = 
    AdHoc { limit::Int }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0037 :: Int -> [Integer]
problem0037 limit = take limit $ filter isReversiblePrime primes

isReversiblePrime :: Integer -> Bool
isReversiblePrime n = all (flip elem primes . read) $ leftTail ++ rightTail
    where
        leftTail = fTail init $ show n
        rightTail = fTail tail $ show n

isReversiblePrimeTest = [
    True @=? isReversiblePrime 3797]

fTail :: ([a] -> [a]) -> [a] -> [[a]]
fTail _ [] = []
fTail f as = as:(fTail f $ f as)

fTailTest = [
    ["12345", "1234", "123", "12", "1"] @=? fTail init "12345",
    ["12345", "2345", "345", "45", "5"] @=? fTail tail "12345",
    ["1"] @=? fTail tail "1",
    [] @=? fTail tail ""]

primes :: [Integer]
primes = 2 : [n | n <- [3,5..], isPrime n]

primesTest = [
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] @=? (take 10 primes),
    [2, 3] @=? (take 2 primes),
    2 @=? (head primes)]

isPrime :: Integer -> Bool
isPrime 0 = False
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
    primesTest ++
    isPrimeTest ++
    fTailTest ++
    isReversiblePrimeTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0037 limit
    printf "The first %d truncatable primes are:\n" limit
    mapM_ (printf "%d\n")  answer
exec Euler = do
    let answer = sum $ problem0037 11
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ limit = 100 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
