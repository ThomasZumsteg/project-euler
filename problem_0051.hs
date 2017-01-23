{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (nub)
import qualified Data.Map as M

data EulerArgs =
    Euler
    | AdHoc{ len::Int }
    | UnitTest
    deriving (Show, Data, Typeable)

-- By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
-- By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.
-- Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

problem0051 :: Int -> [String]
problem0051 l = head $ filter ((==l) . length) $ rollUp M.empty $ concatMap ((patters '.') . show) primes

rollUp :: (Ord a) => M.Map a [b] -> [(a, b)] -> [[b]]
rollUp _ [] = []
rollUp cache ((k, v):vks) = vs : (rollUp cache' vks)
    where
        vs = v : M.findWithDefault [] k cache
        cache' = M.insertWith (++) k [v] cache

rollUpTest = [
    [] @=? rollUp M.empty ([]::[(Int,Int)]),
    [[1],[2],[3,1],[4,2]] @=? rollUp M.empty [(1,1),(0,2),(1,3),(0,4)]]

replace :: (Eq a) => a -> a -> [a] -> [a]
replace r c text = map (\s -> if s == r then c else s) text

replaceTest = [
    "" @=? replace 'a' '.' "",
    ".b." @=? replace 'a' '.' "aba",
    "." @=? replace 'a' '.' "a"]

patters :: (Eq a) => a -> [a] -> [([a], [a])]
patters marker text = [(replace i marker text, text) | i <- nub text]

pattersTest = [
    [(".","1")] @=? patters '.' "1",
    [("..","11")] @=? patters '.' "11",
    [(".2.","121"), ("1.1","121")] @=? patters '.' "121",
    [(".23.","1231"), ("1.31","1231"),("12.1","1231")] @=? patters '.' "1231"]

primes :: [Integer]
primes = 2 : [n | n <- [3,5..], isPrime n]

primesTest = [
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] @=? (take 10 primes),
    [2, 3] @=? (take 2 primes),
    2 @=? (head primes)]

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = all noRemainer primes'
    where
        primes' = takeWhile (\p -> p * p <= n) primes
        noRemainer d = 0 /= rem n d

isPrimeTest = [
    assertBool "9 is not prime" (not $ isPrime 9),
    assertBool "6 is not prime" (not $ isPrime 6),
    assertBool "4 is not prime" (not $ isPrime 4),
    assertBool "0 is not prime" (not $ isPrime 0),
    assertBool "3 is prime" (isPrime 3),
    assertBool "2 is prime" (isPrime 2)]

unitTests = map TestCase $
    isPrimeTest ++
    rollUpTest ++
    replaceTest ++
    pattersTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = problem0051 8
    printf "Answer: %s\n" (last answer)
exec AdHoc{..} = do
    let answer = problem0051 len
    printf "Answer: %s\n" (last answer)
    print answer
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ len=7 },
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
