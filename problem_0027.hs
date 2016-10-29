{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Euler discovered the remarkable quadratic formula:
-- n2+n+41
-- It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39
-- . However, when n=40,402+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,412+41+41
-- is clearly divisible by 41.
-- The incredible formula n2−79n+1601
-- was discovered, which produces 80 primes for the consecutive values 0≤n≤79
-- . The product of the coefficients, −79 and 1601, is −126479.
-- Considering quadratics of the form:
--     n2+an+b
-- , where |a|<1000 and |b|≤1000
-- where |n|
-- is the modulus/absolute value of n
-- e.g. |11|=11 and |−4|=4
-- Find the product of the coefficients, a
-- and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n=0.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Maybe (fromJust, isJust)
import Data.List (findIndex)

data EulerArgs = 
    AdHoc { lower::Integer, upper::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

data Coefficents = Coefficents { b::Integer, c::Integer }
    deriving (Show)

type State = Maybe (Int, Coefficents)

problem0027 :: Integer -> Integer -> (Int, Coefficents)
problem0027 lower upper
    | isJust result = fromJust result
    | otherwise = (0, Coefficents 0 0)
    where
        result = foldl foldCoefficents Nothing
            [Coefficents b c | b <- [lower..upper], c <- [lower..upper]]

foldCoefficents :: State -> Coefficents -> State
foldCoefficents s c 
    | isJust s && c_val < (fst $ fromJust s) = s
    | otherwise = Just (c_val, c)
    where
        c_val = primeSequenceLength c

primeSequenceLength :: Coefficents -> Int
primeSequenceLength (Coefficents b c) = fromJust $ findIndex (not . isPrime . equation) [0..]
    where
        equation n = n ^ 2 + b * n + c

primeSequenceLengthTest = [
    12 @=? primeSequenceLength (Coefficents (-10) (-10)),
    1 @=? primeSequenceLength (Coefficents 5 2),
    80 @=? primeSequenceLength (Coefficents (-79) 1601),
    40 @=? primeSequenceLength (Coefficents 1 41),
    0 @=? primeSequenceLength (Coefficents 0 0)]

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
    primeSequenceLengthTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let (consec, answer) = problem0027 lower upper
    printf ("For coefficnets in [%d, %d] the greates consecutive primes is %d " ++
        "using the equation n^2 + %d*n + %d\n") lower upper consec (b answer) (c answer)
exec Euler = do
    let answer = snd $ problem0027 (-1000) 1000
    printf "Answer: %d\n" ((b answer) * (c answer))
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ lower = 1, upper = 10 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
