{-# LANGUAGE DeriveDataTypeable #-}

-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Text.Printf
import System.Console.CmdArgs
import Data.Time
import Data.List (sort)

data EulerArgs = EulerArgs { 
    smallFactor :: Integer,
    largeFactor :: Integer }
        deriving (Show, Data, Typeable)
    
euler = EulerArgs { 
    smallFactor = 100,
    largeFactor = 999 }

problem0004 :: Integer -> Integer -> Integer
problem0004 start stop = head $ filter (isPalindrome . show) nums
    where
        nums = reverse $ sort [a * b | a <- [start..stop], b <- [start..stop]]

isPalindrome :: String -> Bool
isPalindrome word = word == reverse word

main :: IO ()
main = do
    args <- cmdArgs euler
    let EulerArgs{ smallFactor = small, largeFactor = large } = args
    start <- getCurrentTime
    print $ problem0004 small large
    stop <- getCurrentTime
    print $ diffUTCTime stop start

