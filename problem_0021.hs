{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

-- Evaluate the sum of all the amicable numbers under 10000.

import Common
import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import qualified Data.Map.Lazy as M

problem0021 :: Integer -> Integer
problem0021 limit = foldl addPairs 0 $ takeWhile belowLimit amicablePairs
    where
        addPairs acc (a, b) = acc + a + b 
        belowLimit (a, b) = a < limit && b < limit

problem0021Test = map TestCase [ ] 

amicablePairs :: (Integral a) => [(a, a)]
amicablePairs = amicablePairsWorker M.empty 1

amicablePairsTest = map TestCase [ ]

amicablePairsWorker :: (Integral a) => M.Map a [a] -> a -> [(a, a)]
amicablePairsWorker cache n = pairs ++ more
    where
        factorSum = sum $ factors n
        pairs = map (\n' -> (n', n)) (M.findWithDefault [] factorSum cache)
        more = amicablePairsWorker cache' (n+1)
        cache' =  if M.member factorSum cache
            then M.insertWith (++) factorSum [n] cache
            else M.insert factorSum [n] cache

amicablePairsWorkerTest = map TestCase [ 
    [(1, 2)] @=? (take 1 $ amicablePairsWorker M.empty 1),
    assertBool "Contains (29, 43)" $ elem (29, 43) $ amicablePairsWorker M.empty 1
    ]

factors :: (Integral a) => a -> [a]
factors 1 = [1]
factors n = filter ((==) 0 . mod n) [1..(n-1)]

factorsTest :: [Test]
factorsTest = map TestCase [
    [1] @=? factors 1,
    [1] @=? factors 2,
    [1] @=? factors 3,
    [1,2] @=? factors 4,
    [1,2,4,5,10,11,20,22,44,55,110] @=? factors 220,
    [1,2,3,4,6] @=? factors 12,
    [1,2,4,8] @=? factors 16,
    [1] @=? factors 29,
    [1] @=? factors 43,
    (sum $ factors 1) @=? (sum $ factors 2),
    (sum $ factors 29) @=? (sum $ factors 43)
    ]

unitTests = problem0021Test ++ 
    amicablePairsWorkerTest ++ 
    amicablePairsTest ++ 
    factorsTest

main = euler_main $ EulerFuncs {
    problem = problem0021,
    euler = problem0021 10000,
    tests = unitTests,
    message = "Hello world: %d",
    defaults = [AdHoc 10000, Euler, UnitTest]
    }

