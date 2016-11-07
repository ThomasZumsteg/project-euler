{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
--     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:
--     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- How many different ways can £2 be made using any number of coins?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (sortBy)

data EulerArgs = 
    AdHoc { total::Integer, coins::String }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0031 :: [Integer] -> Integer -> Int
problem0031 coins total = length $ makeChange coins' total
    where
        coins' = sortBy (flip compare) coins

makeChange :: [Integer] -> Integer -> [[Integer]]
makeChange [] _ = []
makeChange coins@(c:cs) total 
    | c > total = makeChange cs total
    | c == total = [[c]] ++ withOutCoin
    | otherwise = withCoin ++ withOutCoin
    where
        withCoin = map ((:) c) $ makeChange coins (total - c)
        withOutCoin = makeChange cs total

makeChangeTest = [
    [[5,1,1],[3,3,1],[3,1,1,1,1],[1,1,1,1,1,1,1]] @=? makeChange [5,3,1] 7,
    [[2],[1,1]] @=? makeChange [3,2,1] 2,
    [[1]] @=? makeChange [3,2,1] 1,
    [] @=? makeChange [2] 1,
    [[1]] @=? makeChange [1] 1,
    [] @=? makeChange [] 0]

unitTests = map TestCase $
    makeChangeTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let coins' = sortBy (flip compare) $ read coins 
        answer = makeChange coins' total
    printf "To make change for %d using the coins:\n%s\n" total (show coins')
    printf "There are %d ways\n" (length answer) 
    mapM_ (\(n, a) -> printf "%4d: %s\n" n (show a)) $ zip [(1::Int)..] answer
exec Euler = do
    let answer = problem0031 [1, 2, 5, 10, 20, 50, 100, 200] 200
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        AdHoc{ total=200,coins="[1,2,5,10,20,50,100,200]"},
        Euler,
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
