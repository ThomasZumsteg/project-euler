{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

-- Evaluate the sum of all the amicable numbers under 10000.

import Common
import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import qualified Data.Map.Lazy as M

problem0021 :: Integer -> Integer
problem0021 limit = sum $ filter isAmicable [1..limit]

problem0021Test = map TestCase [ 
    ( sum [220,284,1184,1210,2620] ) @=? problem0021 2620,
    ( sum [220,284] ) @=? problem0021 300,
    ( sum [] ) @=? problem0021 0
    ] 

isAmicable :: (Integral a) => a -> Bool
isAmicable n = (d $ d n) == n && (d n) /= n
    where
        d = sum . factors

isAmicableTest = map TestCase [
    assertBool "220 is Amicable" $ isAmicable 220,
    assertBool "6 is no Amicable" $ not $ isAmicable 6,
    assertBool "110 is not Amicable" $ not $ isAmicable 110
    ]

factors :: (Integral a) => a -> [a]
factors n = 1 : (factorWorker 2)
    where
        factorWorker factor 
            | n < factor^2 = []
            | n == factor^2 = [factor]
            | remainer == 0 = [factor] ++ (factorWorker (factor+1)) ++ [quotent]
            | otherwise = factorWorker (factor+1)
            where 
                (quotent, remainer) = divMod n factor

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
    isAmicableTest ++
    factorsTest

main = euler_main $ EulerFuncs {
    problem = problem0021,
    euler = problem0021 10000,
    tests = unitTests,
    message = "Hello world: %d\n",
    defaults = [AdHoc 10000, Euler, UnitTest]
    }

