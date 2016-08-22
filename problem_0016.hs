{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf (printf)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (IOMode( ReadMode), hGetContents, openFile)
import Data.Char (digitToInt)

-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 2^1000?

problem0016 :: (Integral a) => Integer -> a -> Integer
problem0016 n e = sum $ digits $ n ^ e

digits :: (Num a, Read a, Show a) => a -> [a]
digits = map (\d -> read [d]) . show

problem0016Test :: [Test]
problem0016Test = map TestCase [
     1 @=? problem0016 2 0,
    26 @=? problem0016 2 15
    ]

digitsTest :: [Test]
digitsTest = map TestCase [
    [0] @=? digits 0,
    [1] @=? digits 1,
    [1,1] @=? digits 11,
    [1,2,3,4] @=? digits 1234,
    [3,2,7,6,8] @=? digits 32768
    ]

data EulerArgs = 
    AdHoc { base :: Integer, exponent :: Integer }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = problem0016 base exponent
    printf "The sum of the digits of %d^%d is: %d\n" base exponent answer
exec Euler = do
    let answer = problem0016 2 1000
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList $ problem0016Test ++ digitsTest
    return ()

adHoc = AdHoc{ base = 2, exponent = 15 }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
