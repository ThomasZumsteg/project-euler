{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Maybe (isJust)
import Data.List (permutations, find)

data Triplet = Triplet { a::Integer, b::Integer, c::Integer }
    deriving (Show, Eq)

data EulerArgs = 
    AdHoc{start::Int, items::Maybe Int}
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0032 :: [Triplet]
problem0032 = filter isProduct pandigitalTriplets
    where
        isProduct Triplet{a=a,b=b,c=c} = c == (a * b)
        
pandigitalTriplets :: [Triplet]
pandigitalTriplets = [Triplet a b c | 
    perm <- permutations digits,
    n <- [1..((length perm)-2)],
    m <- [1..((length perm)-1-n)],
    let a = read $ take n perm,
    let b = read $ take m $ drop n perm,
    let c = read $ drop (n + m) perm]
    where
        digits = "123456789"

pandigitalTripletsTest = [
    (Triplet 1 2 3456789) @=? (head $ drop 0 pandigitalTriplets),
    (Triplet 1 23 456789) @=? (head $ drop 1 pandigitalTriplets),
    (Triplet 1 234 56789) @=? (head $ drop 2 pandigitalTriplets),
    (Triplet 1 2345 6789) @=? (head $ drop 3 pandigitalTriplets),
    (Triplet 1 23456 789) @=? (head $ drop 4 pandigitalTriplets),
    (Triplet 1 234567 89) @=? (head $ drop 5 pandigitalTriplets),
    (Triplet 1 2345678 9) @=? (head $ drop 6 pandigitalTriplets),
    (Triplet 12 3 456789) @=? (head $ drop 7 pandigitalTriplets),
    (Triplet 12 34 56789) @=? (head $ drop 8 pandigitalTriplets)]

unitTests = map TestCase $
    pandigitalTripletsTest
    

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = take 10 problem0032
    printf "Answer: %s" (show answer)
exec Euler = do
    let answer = problem0032 
    printf "Answer: %s\n" (show answer)
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        AdHoc{start = 0, items = Nothing},
        Euler,
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
