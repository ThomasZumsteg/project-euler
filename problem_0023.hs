{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

-- A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf, PrintfArg)
import System.Console.CmdArgs
import qualified Data.Map as M
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs = 
    AdHoc { limit::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0023 :: Integer -> Integer
problem0023 limit = error "Not implemented"

abundantPairs :: [(Integer, Integer)]
abundantPairs = error "Not implemented"

properDivisors :: Integer -> [Integer]
properDivisors 1 = [1]
properDivisors n = filter (\f -> rem n f == 0) [1..(n-1)]

properDivisorsTest = map TestCase [
    [1,2,3,4,6] @=? properDivisors 12,
    [1,2] @=? properDivisors 4,
    [1] @=? properDivisors 3,
    [1] @=? properDivisors 2,
    [1] @=? properDivisors 1
    ]

primeFactors :: Integer -> Integer -> [Integer]
primeFactors n d  
    | n == 1 = []
    | d < 2 = error "Divisor needs to be 2 or larger"
    | d * d > n = [n]
    | d /= 2 && r == 0 = d : primeFactors q d
    | d /= 2 && r /= 0 = primeFactors n (d + 2)
    | d == 2 && even n = 2 : primeFactors (div n 2) 2 
    | otherwise = primeFactors n 3
    where
        (q, r) = divMod n d

primeFactorsTest = map TestCase [
    [97] @=? primeFactors 97 2,
    [3, 37] @=? primeFactors 111 2,
    [2,2,5,5] @=? primeFactors 100 2,
    [2,2,3] @=? primeFactors 12 2,
    [2,3] @=? primeFactors 6 2,
    [5] @=? primeFactors 5 2,
    [2,2] @=? primeFactors 4 2,
    [3] @=? primeFactors 3 2,
    [2] @=? primeFactors 2 2,
    [] @=? primeFactors 1 2
    ]

unitTests = primeFactorsTest ++
    properDivisorsTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0023 limit
    printf  "The sum of all non-abundant numbers below %s is %s" limit answer
exec Euler = do
    let answer = problem0023 28123 
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ limit = 28123 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
