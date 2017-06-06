{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf, PrintfArg, formatArg, formatString)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

import Data.List (maximumBy)
import qualified Data.Set as S

-- Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of numbers less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
--  n   Relatively Prime  φ(n)  n/φ(n)
--  2   1                 1     2
--  3   1,2               2     1.5
--  4   1,3               2     2
--  5   1,2,3,4           4     1.25
--  6   1,5               2     3
--  7   1,2,3,4,5,6       6     1.1666...
--  8   1,3,5,7           4     2
--  9   1,2,4,5,7,8       6     1.5
--  10  1,3,7,9           4     2.5

-- It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.
-- Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

problem0069 :: Int -> (Int, S.Set Integer)
problem0069 n = maximumBy size $ take n $ zip [0..] relativePrimes
    where
        size (ia, sa) (ib, sb) = compare ((S.size sb) * ia) ((S.size sa) * ib)

relativePrimes :: [S.Set Integer]
relativePrimes = [S.fromList [n | n <- [1..i], 1 == gcd i n] | i <- [0..]]

relativePrimesTest = [
    S.fromList []    @=? (relativePrimes !! 0),
    S.fromList [1]   @=? (relativePrimes !! 1),
    S.fromList [1]   @=? (relativePrimes !! 2),
    S.fromList [1,2] @=? (relativePrimes !! 3)
    ]

unitTests = map TestCase $
    relativePrimesTest

data Arg = Euler | UnitTest |
    AdHoc { limit::Int } 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = problem0069 1000000
        printf "Answer %s\n" (show answer)
    exec AdHoc {..} = do
        let answer = problem0069 limit
        printf "Answer %d: %d\n" (fst answer) (S.size $ snd answer)
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { limit= 10 }]

