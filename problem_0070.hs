{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf, PrintfArg, formatArg, formatString)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main, primes)

import Data.List (minimumBy, sort)

-- Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
-- The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
-- Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
-- Find the value of n, 1 < n < 10⁷, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum

data Problem = Problem { pLowerLimit :: Integer, pUpperLimit :: Integer }
data AdHocReturn = AdHocReturn { num :: Integer, rPrimes :: [Integer] }

instance PrintfArg AdHocReturn where
    formatArg (AdHocReturn { num = n, rPrimes = rps }) = formatString $
            ((printf "%7d:%7d - %4.2f %s" n len frac (show rps))::String)
        where
            len = toInteger $ length rps
            frac = ((fromInteger n) / (fromInteger len))::Double

problem :: Problem -> Integer
problem (Problem lower upper) = minimumBy nOverPhi $ 
    filter permutations [lower..upper]
    where
        size = toInteger . length . phi
        permutations n = (sort $ show n) == (sort $ show $ size n)
        nOverPhi a b = compare (a * size b) (b * size a) 

adhoc :: Arg -> [AdHocReturn]
adhoc (AdHoc start stop) = [AdHocReturn n p | 
    n <- [start..stop],
    let p = phi n]

phi :: Integer -> [Integer]
phi 0 = []
phi 1 = []
phi n = 1 : [i | i <- [2..(n-1)], 1 == gcd i n]

phiTest = [
    [1] @=? phi 1,
    [1] @=? phi 2,
    [1,2] @=? phi 3,
    [1,3] @=? phi 4,
    [1,2,3,4] @=? phi 5,
    [1,5] @=? phi 6,
    [1,2,3,4,5,6] @=? phi 7,
    [1,3,5,7] @=? phi 8,
    [1,2,4,5,7,8] @=? phi 9,
    [1,3,7,9] @=? phi 10
    ]

primeFactorsList :: [[Integer]]
primeFactorsList = []  : [] : (worker 2 $ repeat [])
    where
        worker i (x:xs) = error "Not Implemented"

primeFactorsListTest = [
    [] @=? primeFactorsList !! 0,
    [] @=? primeFactorsList !! 1,
    [2] @=? primeFactorsList !! 2,
    [3] @=? primeFactorsList !! 3,
    [2,2] @=? primeFactorsList !! 4,
    [5] @=? primeFactorsList !! 5,
    [2,3] @=? primeFactorsList !! 6,
    [7] @=? primeFactorsList !! 7,
    [2,2,2] @=? primeFactorsList !! 8,
    [3,3] @=? primeFactorsList !! 9,
    [2,5] @=? primeFactorsList !! 10
    ]

mapNth :: Int -> (a -> a) -> [a] -> [a]
mapNth n f xs = worker 1 xs
    where
        worker i (x:xs) = x' : worker i' xs
            where 
                x' = if i == 0 then f x else x
                i' = if i == n then 0 else i + 1

unitTests = map TestCase $
    phiTest

data Arg = Euler | UnitTest |
    AdHoc { adhocLowerLimit::Integer, adhocUpperLimit::Integer } 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = problem (Problem { pLowerLimit = 1, pUpperLimit = 10^7 })
        printf "Answer %d\n" answer
    exec args@(AdHoc {..}) = do
        let answers = adhoc args
        mapM_ (printf "%v\n") answers
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { adhocLowerLimit = 1, adhocUpperLimit = 107 }]
