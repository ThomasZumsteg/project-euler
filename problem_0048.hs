{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

data EulerArgs = 
    Euler 
    | AdHoc{ limit::Integer, digits::Int }
    | UnitTest
    deriving (Show, Data, Typeable)

problem0048 :: Integer -> Int -> Integer
problem0048 limit n = read $ last n $ show $ sum [n^n | n <- [1..limit]]
    where
        last n xs = reverse $ take n $ reverse xs

unitTests = map TestCase $
    []

exec :: EulerArgs -> IO ()
exec Euler = do
    let answer = problem0048 1000 10
    printf "Answer: %d\n" answer
exec AdHoc{..} = do
    print $ problem0048 limit digits
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ limit = 10, digits = 11 },
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
