{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
-- What is the largest n-digit pandigital prime that exists?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (sort)
import Data.Maybe (isJust, fromJust)

data EulerArgs = 
    AdHoc { limit::Integer}
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0041 :: Integer -> [Integer]
problem0041 n = take (fromIntegral n) $ filter isPrime $ map read perms
    where
        perms = concatMap sortedPermutations [[d,(pred d)..'1'] | d <- "987654321"]

pandigital :: String -> Bool
pandigital n 
    | 0 >= l || l > 9 = False
    | otherwise = n' == sorted_n
    where
        sorted_n = sort n
        l = length n
        n' = ['1'..(head $ show l)]

pandigitalTest = [
    True @=? pandigital "123456789",
    True @=? pandigital "12345",
    False @=? pandigital "12346",
    False @=? pandigital "1234567891",
    False @=? pandigital "",
    False @=? pandigital "2",
    True @=? pandigital "1",
    True @=? pandigital "312"]

sortedPermutations :: [a] -> [[a]]
sortedPermutations [] = []
sortedPermutations (c:[]) = [[c]]
sortedPermutations cs = concatMap joinPerms seperated
    where
        seperated = [splitAt n cs | n <- [0..(length cs - 1)]]
        joinPerms (as,(b:bs)) = map ((:) b) $ sortedPermutations (as ++ bs)

sortedPermutationsTest = [
    "123456789" @=? (last $ sortedPermutations "987654321"),
    "987654321" @=? (last $ sortedPermutations "123456789"),
    ["123", "132", "213", "231", "312", "321"] @=? sortedPermutations "123",
    ["12", "21"] @=? sortedPermutations "12",
    ["1"] @=? sortedPermutations "1",
    [] @=? sortedPermutations ""]

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
    primesTest ++
    isPrimeTest ++
    pandigitalTest ++
    sortedPermutationsTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0041 limit
    printf "The largest %d pandigital primes are:\n" limit
    mapM_ (printf "%d\n")  answer
exec Euler = do
    let answer = head $ problem0041 1
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ limit = 10}, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
