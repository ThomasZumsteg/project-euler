{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)


data EulerArgs =
    Euler
    | AdHoc{ limit::Integer }
    | UnitTest
    deriving (Show, Data, Typeable)

-- The prime 41, can be written as the sum of six consecutive primes:
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

problem0050 :: Integer -> Integer
problem0050 = error "Not Implemented"

maxBy :: (Ord b) => (a -> b) -> [a] -> a
maxBy _ (x:[]) = x
maxBy f (x:xs) = case compare (f x) (f x') of
    LT -> x'
    otherwise -> x
    where
        x' = maxBy f xs

maxByTest = [
    [3,4] @=? maxBy sum [[1,2,3],[3,4],[1,1,1]],
    [1,2,3] @=? maxBy length [[3,4],[1,2,3]],
    [1,2,3] @=? maxBy length [[1,2,3],[3,4]],
    [3,4] @=? maxBy sum [[1,2,3],[3,4]],
    [1,2,3] @=? maxBy sum [[1,2,3]]]

primeSequenceSums :: [[Integer]]
primeSequenceSums = [ps | p <- primes, 
    let ps = primeSequence p, not $ null ps]

primeSequence :: Integer -> [Integer]
primeSequence prime = worker [] primes
    where
        worker _ [] = error "Ran out of primes"
        worker [] (p:ps) 
            | prime <= (p * 2) = []
            | otherwise = worker [p] ps
        worker seq@(_:seq') ps@(p:ps') = case compare (sum seq) prime of
            LT -> worker (seq ++ [p]) ps'
            EQ -> if length seq == 1 then [] else seq
            _ -> worker seq' ps

primeSequenceTest = [
    [] @=? primeSequence 13,
    [2,3,5,7,11,13] @=? primeSequence 41]

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
    primeSequenceTest ++
    maxByTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = problem0050 1000000
    printf "Answer: %d\n" answer
exec AdHoc{..} = do
    let seqs = takeWhile (\s -> limit > sum s) primeSequenceSums
    mapM_ (\seq -> printf "%d: %s\n" (sum seq) (show seq)) seqs
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ limit=1000 },
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
