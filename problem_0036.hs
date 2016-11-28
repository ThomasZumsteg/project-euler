{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
-- (Please note that the palindromic number, in either base, may not include leading zeros.)

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs = 
    AdHoc { limit::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0036 :: Integer -> [(Integer, [Integer])]
problem0036 limit = filter bothPaindromes $ map makeDecimalBinary [1..limit]
    where
        binary = toBase 2
        bothPaindromes (d, b) = (isPalindrome b) && (isPalindrome $ show d) 
        makeDecimalBinary n = (n, binary n) 

toBase :: Integer -> Integer -> [Integer]
toBase _ 0 = []
toBase b n = r : (toBase b q)
    where
        (q, r) = divMod n b

toBaseTest = [
    [1,0,0,1,0,0,1,0,0,1] @=? toBase 2 585,
    [1] @=? toBase 2 1,
    [1,1] @=? toBase 2 3,
    [1,1] @=? toBase 3 4]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs)
    | x == (last xs) = isPalindrome $ init xs
    | otherwise = False

isPalindromeTest = [
    False @=? isPalindrome "12",
    True @=? isPalindrome "1",
    True @=? isPalindrome "",
    True @=? isPalindrome "1234321"]

unitTests = map TestCase $
    isPalindromeTest ++
    toBaseTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0036 limit
    printf "Below %d there are %d palindromic in base 10 and 2:\n" limit (length answer)
    mapM_ (\(d, b) -> printf "%4d: %s\n" d (concat $ map show b))  answer
exec Euler = do
    let answer = sum $ map fst $ problem0036 1000000
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
