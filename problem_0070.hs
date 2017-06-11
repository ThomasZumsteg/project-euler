{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf, PrintfArg, formatArg, formatString)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main, primes)

-- Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
-- The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
-- Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
-- Find the value of n, 1 < n < 107, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum

data Problem = Problem { pLowerLimit :: Integer, pUpperLimit :: Integer }
data AdHocReturn = AdHocReturn { num :: Integer, rPrimes :: [Integer] }

instance PrintfArg AdHocReturn where
    formatArg (AdHocReturn { num = n, rPrimes = rps }) = formatString $
        printf "%d\n" n 
        where
            len = toInteger $ length rps
            frac = ((fromInteger n) / (fromInteger len))::Double

problem :: Problem -> Integer
problem p = error "Not Implemented"

adhoc :: Arg -> [AdHocReturn]
adhoc = error "Not Implemented"

phi :: Integer -> [Integer]
phi n = 1 : [i | i <- [1..(n-1)], 1 /= gcd i n]

phiTest = [
    [1] @=? phi 1,
    [1] @=? phi 2,
    [1,2] @=? phi 3,
    [1,3] @=? phi 4,
    [1,2,3,4,5] @=? phi 5,
    [1,2,3,4,5] @=? phi 6,
    [1,7] @=? phi 7,
    [1,3,5,7] @=? phi 8,
    [1,2,4,5,7,8] @=? phi 9,
    [1,2,5,7] @=? phi 10
    ]

unitTests = map TestCase $
    phiTest

data Arg = Euler | UnitTest |
    AdHoc { adhocLowerLimit::Integer, adhocUpperLimit::Integer } 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = problem (Problem { pLowerLimit = 1, pUpperLimit = 107 })
        printf "Answer %d\n" answer
    exec args@(AdHoc {..}) = do
        let answers = adhoc args
        mapM_ (printf "%v\n") answers
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { adhocLowerLimit = 1, adhocUpperLimit = 10 }]

