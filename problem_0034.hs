{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

data EulerArgs = 
    AdHoc { limit :: Maybe Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0034 :: Maybe Integer -> [Integer]
problem0034 l = filter (\n -> digitFactorSum n == n) [3..limit]
    where
        biggest = head [n' | n <- [1..], let n' = (10^n) - 1, n' > digitFactorSum n']
        limit = fromMaybe biggest l

digitFactorSum :: Integer -> Integer
digitFactorSum = sum . (map (factorial . toInteger . digitToInt)) . show 
    where
        factorial n = product [2..n]

digitFactorSumTest = [
    4 @=? digitFactorSum 120,
    145 @=? digitFactorSum 145,
    2 @=? digitFactorSum 11,
    2 @=? digitFactorSum 2,
    1 @=? digitFactorSum 1]

unitTests = map TestCase $
    digitFactorSumTest

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = problem0034 limit
    printf "List of numbers:\n" 
    mapM_ (printf "%d\n") answer
exec Euler = do
    let answer = sum $ problem0034 Nothing
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        AdHoc{ limit = Nothing },
        Euler,
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
