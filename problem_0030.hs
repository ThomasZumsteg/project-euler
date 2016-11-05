{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
-- 1634 = 1^4 + 6^4 + 3^4 + 4^4
-- 8208 = 8^4 + 2^4 + 0^4 + 8^4
-- 9474 = 9^4 + 4^4 + 7^4 + 4^4
-- As 1 = 1^4 is not a sum it is not included.
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust)
import Data.List (find)

data EulerArgs = 
    AdHoc { nth::Integer, limit::Maybe Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0030 :: Maybe Integer -> Integer -> (Integer, [Integer])
problem0030 userLimit n = (limit, filter (\num -> num == sumNthPower n num) [2..limit])
    where
        upperLimit = fromJust $ find (\num -> num > (sumNthPower n num)) nines
        nines = [read $ replicate i '9' | i <- [1..]]
        limit = if isJust userLimit 
            then (min upperLimit $ fromJust userLimit) 
            else upperLimit
        
sumNthPower :: Integer -> Integer -> Integer
sumNthPower n num = sum $ map (\d -> (toInteger $ digitToInt d) ^ n) $ show num

sumNthPowerTest = [
    1634 @=? sumNthPower 4 1634,
    217 @=? sumNthPower 3 16,
    37 @=? sumNthPower 2 16,
    1 @=? sumNthPower 4 10,
    0 @=? sumNthPower 1 0]

unitTests = map TestCase sumNthPowerTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let (l, answer) = problem0030 limit nth
    printf "The numbers less than %d equal to the sum of their digits to the %dth power are:\n%s\n" l nth (show answer)
exec Euler = do
    let answer = sum $ snd $ problem0030 Nothing 5
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ nth=4, limit=Nothing}, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
