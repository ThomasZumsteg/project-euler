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
import Data.List (find, sort)

data EulerArgs = 
    AdHoc { limit::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0023 :: Integer -> Integer
problem0023 limit = sum $ filter (not . hasPair) [1..limit]

hasPair :: Integer -> Bool
hasPair n = any isAbundant [n - a | a <- pairs]
    where
        pairs = takeWhile ((>=) limit) abundantNums
        limit = div n 2

hasPairTest =  [
    assertBool "1 does not have an abundant pair" $ not $ hasPair 1,
    assertBool "24 has an abundant pair (12,12)" $ hasPair 24,
    assertBool "20 does not have an abundant pair" $ not $ hasPair 20
    ]

isAbundant :: Integer -> Bool
isAbundant a = a < (sum $ properDivisors a)

isAbundantTest = [
    assertBool "12 is abundant" $ isAbundant 12,
    assertBool "13 is not abundant" $ not $ isAbundant 13
    ]

abundantPairs :: [(Integer, Integer)]
abundantPairs = concat [[(a, b) | a <- subset b] | b <- abundantNums]
    where
        subset s = takeWhile ((>=) s) abundantNums

abundantPairsTest =  [
    (12, 12) @=? head abundantPairs,
    (12, 18) @=? (head $ drop 1 abundantPairs),
    (18, 18) @=? (head $ drop 2 abundantPairs),
    (12, 30) @=? (head $ drop 10 abundantPairs)
    ]

abundantNums :: [Integer]
abundantNums = filter isAbundant [1..28124]

abundantNumsTest =  [
    [12,18,20,24,30,36,40,42,48,54] @=? take 10 abundantNums,
    [12,18] @=? take 2 abundantNums,
    [12] @=? take 1 abundantNums
    ]

properDivisors :: Integer -> [Integer]
properDivisors 1 = [1]
properDivisors n = filter (\f -> rem n f == 0) [1..(n-1)]

properDivisorsTest =  [
    [1,2,3,4,6] @=? properDivisors 12,
    [1,2] @=? properDivisors 4,
    [1] @=? properDivisors 3,
    [1] @=? properDivisors 2,
    [1] @=? properDivisors 1
    ]

primeFactors :: Integer -> [Integer]
primeFactors n 
    | n <= 1 = []
    | n == 2 = [2]
    | isNothing divisor = [n]
    | otherwise = primeFactors d' ++ primeFactors (div n d')
        where
            sqrtN = ceiling $ sqrt $ fromIntegral n 
            divisor = find (\d -> 0 == rem n d) [sqrtN, sqrtN - 1..2]
            d' = fromJust divisor

primeFactorsTest =  [
    [97] @=? (sort $ primeFactors 97),
    [3, 37] @=? (sort $ primeFactors 111),
    [2,2,5,5] @=? (sort $ primeFactors 100),
    [2,2,3] @=? (sort $ primeFactors 12),
    [2,3] @=? (sort $ primeFactors 6),
    [5] @=? primeFactors 5,
    [2,2] @=? primeFactors 4,
    [3] @=? primeFactors 3,
    [2] @=? primeFactors 2,
    [] @=? primeFactors 1
    ]

unitTests = map TestCase $
    primeFactorsTest ++
    properDivisorsTest ++
    abundantPairsTest ++
    abundantNumsTest ++
    isAbundantTest ++
    hasPairTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0023 limit
    printf  "The sum of all non-abundant numbers below %d is %d\n" limit answer
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
