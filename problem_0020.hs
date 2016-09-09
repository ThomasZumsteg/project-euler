{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- n! means n × (n − 1) × ... × 3 × 2 × 1

-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

-- Find the sum of the digits in the number 100!

import Text.Printf (printf, PrintfArg)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

problem0020 :: Integer -> Integer
problem0020 = sumDigits . factorial
    where
        factorial n = foldl1 (*) [1..n]
        sumDigits = sum . map (toInteger . digitToInt) . show

problem0020Test :: [Test]
problem0020Test = map TestCase [
    36 @=? problem0020 11,
    27 @=? problem0020 10,
     3 @=? problem0020  5,
     6 @=? problem0020  4,
     6 @=? problem0020  3,
     2 @=? problem0020  2,
     1 @=? problem0020  1
    ]

data EulerArgs = 
    AdHoc { number :: Integer }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = problem0020 number
    printf "The sum of the digits of %d factoral is: %d\n" number answer
exec Euler = do
    let answer = problem0020 100
    printf "Answer: %d\n" answer 

exec UnitTest = do 
    runTestTT $ TestList $ problem0020Test
    return ()

adHoc = AdHoc{ number = 100 }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start

