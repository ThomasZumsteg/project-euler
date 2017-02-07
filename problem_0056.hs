{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Char (digitToInt)

-- A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.
-- Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

data EulerArgs =
    Euler
    | AdHoc { limit::Integer }
    | UnitTest
    deriving (Show, Data, Typeable)

data DigitalPair = DigitalPair { a::Integer, b::Integer }

problem0056 :: Integer -> Integer
problem0056 limit = maximum $ map digitialSum $ digitalPairs limit

digitialSum :: DigitalPair -> Integer
digitialSum (DigitalPair a b) = sum $ map (toInteger . digitToInt) $ show $ a ^ b

digitialSumTest = [
    1 @=? digitialSum (DigitalPair 10 100),
    1 @=? digitialSum (DigitalPair 100 100)]

digitalPairs :: Integer -> [DigitalPair]
digitalPairs limit = [DigitalPair a b | a <- [0..limit], b <- [0..limit]]

unitTests = map TestCase $ 
    digitialSumTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = problem0056 100
    printf "Answer: %d\n" answer
exec AdHoc{..} = do
    let  answer = digitalPairs limit
    mapM_ (\d@(DigitalPair a b) -> printf "%d ^ %d: %d\n" a b (digitialSum d)) answer
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc {limit = 10},
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
