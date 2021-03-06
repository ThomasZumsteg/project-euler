{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- Let us list the factors of the first seven triangle numbers:
--      1: 1
--      3: 1,3
--      6: 1,2,3,6
--     10: 1,2,5,10
--     15: 1,3,5,15
--     21: 1,3,7,21
--     28: 1,2,4,7,14,28
-- We can see that 28 is the first triangle number to have over five divisors.
-- What is the value of the first triangle number to have over five hundred divisors?

import Text.Printf (printf)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (find, sort, group, delete)
import Data.Maybe (fromJust)

problem0012 :: (Integral a, Ord a) => a -> a
problem0012 n = triangleNums !! firstLargerThen
    where
        primeFactorsOfT t = delete 2 $ primeFactors t ++ primeFactors (t+1)
        firstLargerThen = pred $ fromJust $ find ((<=) n . fromIntegral . totalFactors . primeFactorsOfT) [1..]

totalFactors :: (Integral a) => [a] -> Int
totalFactors = product . map ((+) 1 . length) . group . sort 

factors :: (Integral a) => a -> [a]
factors n | n <= 0 = error "n cannot be a negative number"
factors n = [f | f <- [1..n], 0 == rem n f]

primeFactors :: (Integral a) => a -> [a]
primeFactors 0 = []
primeFactors num = primeFactorsWorker [2..] num
    where
        primeFactorsWorker _ 1 = []
        primeFactorsWorker factors@(f:fs) n 
            | r == 0 = f : primeFactorsWorker factors q
            | f * f > n = [n]
            | otherwise = primeFactorsWorker fs n
            where
                (q, r) = quotRem n f


triangleNums :: (Integral a) => [a]
triangleNums = scanl1 (+) [1..]

problem0012Test :: [Test]
problem0012Test = map TestCase [
     1 @=? problem0012 1,
     3 @=? problem0012 2,
     6 @=? problem0012 3,
     6 @=? problem0012 4,
    28 @=? problem0012 5
    ]

factorsTest :: [Test]
factorsTest = map TestCase [
    [1] @=? factors 1,
    [1,2] @=? factors 2,
    [1,2,4] @=? factors 4,
    [1,2,3,4,6,12] @=? factors 12,
    [1,2,4,7,14,28] @=? factors 28 
    ]

primeFactorsTest :: [Test]
primeFactorsTest = map TestCase [
    [] @=? primeFactors 1,
    [2] @=? primeFactors 2,
    [2,2] @=? primeFactors 4,
    [2,2,3] @=? primeFactors 12,
    [2,2,7] @=? primeFactors 28 
    ]
triangleNumsTest :: [Test]
triangleNumsTest = map TestCase [
    [1,3,6,10,15,21,28,36,45,55] @=? take 10 triangleNums
    ]
    
data EulerArgs = 
    AdHoc { size :: Integer }
    | UnitTest
    | Euler 
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    printf "%d is the first triangle number to have %d factors or more\n" (problem0012 size) size
exec Euler = do
    let result = problem0012 500::Integer
    printf "Answer: %d\n" result
exec UnitTest = do 
    runTestTT $ TestList $ factorsTest ++ triangleNumsTest ++ problem0012Test ++ primeFactorsTest
    return ()

adHoc = AdHoc{ size = 4 }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
