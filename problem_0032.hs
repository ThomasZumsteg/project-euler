{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Maybe (isJust, fromJust, catMaybes)
import Data.List (nub, sort, permutations, find)

data Triplet = Triplet { a::Integer, b::Integer, c::Integer }
    deriving (Show, Eq)

data EulerArgs = 
    AdHoc{start::Int, items::Maybe Int}
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0032 :: [Triplet]
problem0032 = filter multiple (productList "123456789")
    where
        multiple (Triplet a b c) = a * b == c

productList :: String -> [Triplet]
productList digits = [Triplet (read a') (read b') (read c') |
    perm <- permutations digits,
    m <- [1..(l_digits - 2)],
    n <- [(m + 1)..(l_digits - 1)],
    let (a', b', c') = split2 perm m n]
    where
        l_digits = length digits
        split2 items i j = (a, b, c)
            where
                a = take i items
                b = take (j - i) $ drop i items
                c = drop j items 

productListTest = [
    True @=? elem (Triplet 23 45 16) (productList "123456"),
    (Triplet 1 2 3456789) @=? (head $ drop 0 (productList "123456789"))]

unitTests = map TestCase $
    productListTest

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = if isJust items
        then take (fromJust items) $ drop start problem0032
        else drop start problem0032
    printf "List of Pandigital products:\n" 
    mapM_ (\(Triplet a b c) -> printf "%d * %d = %d\n" a b c)  answer
exec Euler = do
    let answer = sum $ nub $ map c problem0032 
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
