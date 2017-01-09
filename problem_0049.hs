{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (sort)

-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
-- What 12-digit number do you form by concatenating the three terms in this sequence?

data EulerArgs =
    Euler
    | AdHoc{ digits::Int }
    | UnitTest
    deriving (Show, Data, Typeable)

problem0049 :: Int -> [(Integer, Integer, Integer)]
problem0049 n_digits = [ (p, p+d, p+2*d) |
    p <- n_digit_primes,
    d <- [1..(stop - 2 * p)],
    all isPrime [p, p + d, p + 2 * d],
    (digits p == digits (p + d)) && (digits p == digits (p + 2 * d))]
    where
        start = 10^(n_digits-1)
        stop = 10^n_digits
        digits = sort . show
        n_digit_primes= takeWhile (<stop) $ dropWhile (<start) primes
        sequences = [(p,p+d,p+2*d) | p <- n_digit_primes, d <- [1..(stop - 2 * p)]]

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
    isPrimeTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let (n1, n2, n3) = head $ drop 1 $ problem0049 4
    printf "Answer: %d%d%d\n" n1 n2 n3
exec AdHoc{..} = do
    mapM_ print (problem0049 digits)
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ digits=4 },
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
