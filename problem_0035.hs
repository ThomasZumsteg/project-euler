{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
-- How many circular primes are there below one million?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs = 
    AdHoc { limit::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0035 :: Integer -> [Integer]
problem0035 limit = filter isCircularPrime $ takeWhile (<limit) primes

isCircularPrime :: Integer -> Bool
isCircularPrime num = all (isPrime . read) rotations
    where
        strNum = show num 
        rotations = map (rotate strNum) [0..(length strNum)]

isCircularPrimeTest = [
    True @=? isCircularPrime 197]

rotate :: [a] -> Int -> [a]
rotate xs n = zipWith const (drop (if n < 0 then n' else n) $ cycle xs) xs
    where
        n' = mod n $ length xs

rotateTest = [
    "3412" @=? rotate "1234" (-10) ,
    "3412" @=? rotate "1234" (-2) ,
    "3412" @=? rotate "1234" 6 ,
    "1234" @=? rotate "1234" 4 ,
    "2341" @=? rotate "1234" 1 ,
    "1234" @=? rotate "1234" 0]

primes :: [Integer]
primes = 2 : [n | n <- [3,5..], isPrime n]

primesTest = [
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] @=? (take 10 primes),
    [2, 3] @=? (take 2 primes),
    2 @=? (head primes)]

isPrime :: Integer -> Bool
isPrime 0 = False
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
    primesTest ++
    rotateTest ++
    isCircularPrimeTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0035 limit
    printf "Below %d there are %d circular primes:" limit (length answer)
    mapM_ print answer
exec Euler = do
    let answer = length $ problem0035 1000000
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ limit = 100 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
