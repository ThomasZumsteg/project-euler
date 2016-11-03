{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:
--     22=4, 23=8, 24=16, 25=32
--     32=9, 33=27, 34=81, 35=243
--     42=16, 43=64, 44=256, 45=1024
--     52=25, 53=125, 54=625, 55=3125
-- If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:
-- 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
-- How many distinct terms are in the sequence generated by ab for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import qualified Data.Set as S

data EulerArgs = 
    AdHoc { upper::Integer, lower::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0029 :: Integer -> Integer -> Int
problem0029 lower upper = length $ S.fromList 
    [a ^ b | a <- [lower..upper], b <- [lower..upper]]

unitTests = map TestCase []

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0029 lower upper
    printf "There are %d unique products in the range [%d, %d]" answer lower upper
exec Euler = do
    let answer = problem0029 2 100
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ lower=2, upper=5 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
