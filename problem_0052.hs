{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (sort)

-- It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

data EulerArgs =
    Euler
    | AdHoc{ limit::Int, multiples::[Integer] }
    | UnitTest
    deriving (Show, Data, Typeable)

problem0052 :: [Integer] -> [[Integer]]
problem0052 multiples = filter allSameChars [map (*m) multiples | m <- [1..]]

allSameChars :: (Show a) => [a] -> Bool
allSameChars (x:xs) = all (sameChars x) xs

allSameCharsTest = [
    True @=? allSameChars [121,211,112]]

sameChars :: (Show a, Show b) => a -> b -> Bool
sameChars x y = (sort $ show x) == (sort $ show y)

sameCharsTest = [
    False @=? sameChars "" 1,
    False @=? sameChars 111 1,
    False @=? sameChars 000 4321,
    True @=? sameChars 1234 3421,
    False @=? sameChars 11234 4321,
    True @=? sameChars 1234 4321]

unitTests = map TestCase $
    sameCharsTest ++
    allSameCharsTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = head $ head $ problem0052 [1..6]
    printf "Answer: %d\n" answer
exec AdHoc{..} = do
    let answer = take limit $ problem0052 multiples
    mapM_ print answer
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ limit=10, multiples=[1..6] },
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
