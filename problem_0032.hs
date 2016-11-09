{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (permutations)

data EulerArgs = 
    AdHoc {digits::String}
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0032 :: String -> Integer
problem0032 = error "Not Implemented"

remainerProduct :: String -> [Integer]
remainerProduct = error "Not Implemeneted"

remainerProductTest = [
    [23, 32, 26, 62, 36, 63] @=? remainerProduct "123"]

productAndRemainer :: [(Integer, String)]
productAndRemainer = [(read product, gs) | n <- [1..(length digits - 2)],
    let gs = drop n digits,
    let g = take n digits,
    product <- permutations g]
    where
        digits = "123456789"

productAndRemainerTest = [ 
    (1,"23456789") @=? (head $ drop 0 productAndRemainer),
    (12,"3456789") @=? (head $ drop 1 productAndRemainer),
    (21,"3456789") @=? (head $ drop 2 productAndRemainer),
    (123,"456789") @=? (head $ drop 3 productAndRemainer),
    (213,"456789") @=? (head $ drop 4 productAndRemainer),
    (321,"456789") @=? (head $ drop 5 productAndRemainer),    
    (231,"456789") @=? (head $ drop 6 productAndRemainer),    
    (312,"456789") @=? (head $ drop 7 productAndRemainer),    
    (132,"456789") @=? (head $ drop 8 productAndRemainer),    
    (1234,"56789") @=? (head $ drop 9 productAndRemainer)]

orderedPerm :: [a] -> [[a]]
orderedPerm = error "Not Implemented"

orderedPermTest = [
    ["123","132","213","231","312","321"] @=? orderedPerm "123",
    ["12","21"] @=? orderedPerm "12",
    ["1"] @=? orderedPerm "1"]

unitTests = map TestCase $
    productAndRemainerTest ++
    orderedPermTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0032 digits
    printf "Answer: %d" answer
exec Euler = do
    let answer = problem0032 "123456789"
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        AdHoc{digits="123"},
        Euler,
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
