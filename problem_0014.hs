{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf (printf)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (IOMode( ReadMode), hGetContents, openFile)
import Data.Char (digitToInt)

-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.

problem0014 :: Int -> Int
problem0014 l = maximum $ map collatzLength [1..l]

collatzLength :: (Integral a) => a -> a
collatzLength 1 = 1
collatzLength n = 1 + if even n then evenVal else oddVal
    where
        evenVal = collatzLength (div n 2)
        oddVal = collatzLength (3 * n + 1)

collatzSequence :: [[Integer]]
collatzSequence = map collatz [1..]

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | r == 0 = n : collatz (div n 2)
    | otherwise = n : collatz (3 * n + 1)
    where
        r = rem n 2

problem0014Test :: [Test]
problem0014Test = map TestCase [
    1 @=? problem0014 1,
    2 @=? problem0014 2,
    8 @=? problem0014 3,
    8 @=? problem0014 4,
    8 @=? problem0014 5
    ]

collatzLengthTest :: [Test]
collatzLengthTest = map (\n -> TestCase ((length $ collatzSequence !! (n-1)) @=? collatzLength n)) [1..10]
    
collatzTest :: [Test]
collatzTest = map TestCase [
    [1] @=? collatz 1,
    [2,1] @=? collatz 2,
    [3,10,5,16,8,4,2,1] @=? collatz 3,
    [4,2,1] @=? collatz 4,
    [5,16,8,4,2,1] @=? collatz 5
    ]

data EulerArgs = 
    AdHoc { limit :: Int }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = problem0014 limit
    printf "The longest Collatz sequence under %d is: %d\n" limit answer
    printf "It has %d entries\n" (length $ collatzSequence !! answer)
exec Euler = do
    let answer = problem0014 (10^6)
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList $ collatzTest ++ problem0014Test ++ collatzLengthTest
    return ()

adHoc = AdHoc{ limit = 100 }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
