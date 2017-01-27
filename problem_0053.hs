{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (nub)

-- There are exactly ten ways of selecting three from five, 12345:
-- 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
-- In combinatorics, we use the notation, 5C3 = 10.
-- In general,
-- nCr = n!/r!(n−r)!, where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
-- It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
-- How many, not necessarily distinct, values of nCr, for 1 ≤ n ≤ 100, are greater than one-million?

data EulerArgs =
    Euler
    | AdHoc{ n::Integer, limit::Integer }
    | UnitTest
    deriving (Show, Data, Typeable)

problem0053 :: Integer -> Integer -> [(Integer,Integer,Integer)]
problem0053 limit bigNum = [(n, r, nCr) | n <- [1..limit], r <- [0..n], 
    let nCr = c n r, nCr > bigNum]

c :: Integer -> Integer -> Integer
c n r = div (fact n) $ (fact r) * (fact (n - r))
    where
        fact = (!!) factorials . fromIntegral

cTest = [
    1144066 @=? c 23 10,
    10 @=? c 5 3]

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

factorialsTest = [
    2 @=? (factorials !! 2) ,
    1 @=? (factorials !! 1) ,
    1 @=? (factorials !! 0)]

unitTests = map TestCase $
    factorialsTest ++
    cTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = problem0053 100 (10 ^ 6)
    printf "Answer: %d\n" (length answer)
exec AdHoc{..} = do
    let answer = problem0053 n (10 ^ 6)
    mapM_ (\(n, r, nCr) -> printf "%d C %d = %d\n" n r nCr) answer
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ n=23, limit=(10^6)},
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
