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

type Cache = M.Map String [Integer]

problem0051 :: Int -> [Integer]
problem0051 l = head $ filter ((>l) . length) $ concatMap primePattern primes

repeateDigitPrimes :: M.Map String [Integer]
repeateDigitPrimes = M.fromListWith (++) primeDigits 
    where
        primeDigits = concatMap keyValues primes
        keyValues p = [(k, [p]) | k <- repeatChars (show p) '.']

repeateDigitPrimesTest = [
    [13,23,43,53,73,83] @=? (M.findWithDefault [] ".3" repeateDigitPrimes)]

slice :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
slice start stop (x:xs)
    | start x && stop x = []
    | start x = x : slice (const True) stop xs
    | otherwise = slice start stop xs

sliceTest = [
    [2,3,4] @=? slice (>1) (>=5) [1..]]

primePattern :: Integer -> [[Integer]]
primePattern p = filter (not . null) [[n | c <- digits, 
    let s = replace str d c,
    let n = read s,
    c /= d,
    '0' /= (head s),
    isPrime n] | d <- (nub str)]
    where
        str = show p
        digits = "0123456789"

primePatternTest = [
    [[797],[101,131,151,181]] @=? primePattern 191,
    [[101,151,181,191]] @=? primePattern 131,
    [[23,43,53,73,83],[11,17,19]] @=? primePattern 13]

replace :: String -> Char -> Char -> String
replace text r c = map (\s -> if s == r then c else s) text

replaceTest = [
    "" @=? replace "" 'a' '.',
    ".b." @=? replace "aba" 'a' '.',
    "." @=? replace "a" 'a' '.']

repeatChars :: String -> Char -> [String]
repeatChars chars r = [map (sub c) chars | c <- uniqueChars]
    where
        uniqueChars = nub chars
        sub c s = if s == c then r else s

repeatCharsTest = [
    [".4023402","6.023.02","64.234.2","640.340.","6402.402"] @=? repeatChars "64023402" '.',
    ["...."] @=? repeatChars "1111" '.',
    [] @=? repeatChars "" '.',
    [".2.","1.1"] @=? repeatChars "121" '.',
    ["."] @=? repeatChars "1" '.']

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
    repeatCharsTest ++
    replaceTest ++
    primePatternTest ++
    sliceTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = problem0051 8
    printf "Answer: %d\n" (head answer)
exec AdHoc{..} = do
    let answer = problem0051 len
    printf "Answer: %d\n" (head answer)
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
