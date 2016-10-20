{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The Fibonacci sequence is defined by the recurrence relation:
--     Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
-- Hence the first 12 terms will be:
--     F1 = 1
--     F2 = 1
--     F3 = 2
--     F4 = 3
--     F5 = 5
--     F6 = 8
--     F7 = 13
--     F8 = 21
--     F9 = 34
--     F10 = 55
--     F11 = 89
--     F12 = 144
-- The 12th term, F12, is the first term to contain three digits.
-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (findIndex)
import Data.Maybe (fromJust)

data EulerArgs = 
    AdHoc { digits::Int }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0025 :: Int -> Int
problem0025 digits = fromJust $ findIndex ((>= digits) . length . show) fibonacci

fibonacci :: [Integer]
fibonacci = 1:1:[fib n | n <- [2..]]
    where
        fib m = (fibonacci !! (m - 1)) + (fibonacci !! (m - 2))

fibonacciTest = [
    [1,1,2,3,5,8,13,21] @=? take 8 fibonacci,
    3 @=? fibonacci !! 3,
    2 @=? fibonacci !! 2,
    1 @=? fibonacci !! 1,
    1 @=? fibonacci !! 0]

unitTests = map TestCase $
    fibonacciTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0025 digits 
    printf "The first Fibonacci number with at least %d digits is at index %d" digits answer
exec Euler = do
    let answer = problem0025 1000
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ digits = 3 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
