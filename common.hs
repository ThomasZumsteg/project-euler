{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Common where

import Text.Printf (printf, PrintfArg)
import Test.HUnit ((@=?), runTestTT, Test(..), assertBool)
import System.Console.CmdArgs
import Data.Map
import Data.Time (getCurrentTime, diffUTCTime)

class EulerArg a where
    exec :: a -> IO ()

euler_main :: (EulerArg a, Data a) => [a] -> IO ()
euler_main eulerModes = do
    args <- cmdArgs $ modes eulerModes
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start

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
