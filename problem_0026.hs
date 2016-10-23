{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
--     1/2    =     0.5
--     1/3    =     0.(3)
--     1/4    =     0.25
--     1/5    =     0.2
--     1/6    =     0.1(6)
--     1/7    =     0.(142857)
--     1/8    =     0.125
--     1/9    =     0.(1)
--     1/10    =     0.1 
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

data EulerArgs = 
    AdHoc { lower::Integer, upper::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0026 :: Integer -> Integer -> Integer
problem0026 = error "Not Implemented"

cycleLength :: (Eq a) => [a] -> [a] -> Int
cycleLength _ [] = 0
cycleLength ns (m:ms)
    | isJust m' = 1 + fromJust m'
    | otherwise = cycleLength (m:ns) ms
    where
        m' = elemIndex m ns

cycleLengthTest = [
    6 @=? cycleLength [] (digitAndRemainer 1 7),
    1 @=? cycleLength [] (digitAndRemainer 1 3),
    10 @=? cycleLength "" "abcdefghijabcde",
    9 @=? cycleLength ([]::[Int]) [1,2,3,4,5,6,7,8,9,1],
    3 @=? cycleLength ([]::[(Int,Int)]) [(1,1), (2,2), (3,3), (1,1)],
    0 @=? cycleLength ([]::[(Int,Int)]) [(1,1), (2,2), (3,3)],
    0 @=? cycleLength ([]::[(Int,Int)]) []]

digitAndRemainer :: Integer -> Integer -> [(Integer, Integer)]
digitAndRemainer n d = (q, r) : if r == 0 then [] else digitAndRemainer (10*r) d
    where
        (q, r) = divMod n d

digitAndRemainerTest = [
    [(0,1),(2,2),(5,0)] @=? digitAndRemainer 1 4,
    [(0,1),(3,1),(3,1),(3,1),(3,1)] @=? (take 5 $ digitAndRemainer 1 3),
    [(0,1), (5,0)] @=? digitAndRemainer 1 2,
    [(1,0)] @=? digitAndRemainer 1 1]

unitTests = map TestCase $
    digitAndRemainerTest ++
    cycleLengthTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0026 lower upper
    printf "The longest repeating sequence from 1/%d to 1/%d is: %d" lower upper answer
exec Euler = do
    let answer = problem0026 1 1000
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ lower = 1, upper = 10 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
