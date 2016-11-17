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
import Data.List (sort, permutations, find)

data Triplet = Triplet { a::Integer, b::Integer, c::Integer }
    deriving (Show, Eq)

data EulerArgs = 
    AdHoc{start::Int, items::Maybe Int}
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0032 :: [Triplet]
problem0032 = catMaybes $ map (find multiple) productList
    where
        multiple (Triplet a b c) = a * b == c

productList :: [[Triplet]]
productList = [[Triplet (read a') (read b') (read c') |
    ab' <- permutations ab,
    (a', b') <- filter notNull $ uniqueGroups ab'] |
    (c, ab) <- filter length' $ uniqueGroups digits,
    c' <- permutations c]
    where
        digits = "123456789"
        length' (as, bs) = (not $ null as) && (2 <= length bs)
        notNull (as, bs) = (not $ null as) && (not $ null bs)

productListTest = [
    (Triplet 1 2 5748396) @=? (head $ last productList),
    True @=? (elem (Triplet 3 456789 12) (head $ drop 2 productList)),
    -- True @=? (all (\t -> c t == 2) $ head $ drop 1 productList),
    (Triplet 2 3456789 1) @=? (head $ head $ drop 0 productList)]

uniqueGroups :: [a] -> [([a],[a])]
uniqueGroups items = map (binaryZip items) [0..l]
    where
        l = 2 ^ (length items) - 1

uniqueGroupsTest = [
    (sort [("","abc"), ("a","bc"),("b","ac"),("ab","c"),("c","ab"),
           ("ac","b"), ("bc","a"),("abc","")]) @=? (sort $ uniqueGroups "abc"),
    (sort [("","abcd"),("a","bcd"),("ab","cd"),("abc","d"),("abcd",""),
           ("abd","c"),("ac","bd"),("acd","b"),("ad","bc"),("b","acd"),
           ("bc","ad"),("bcd","a"),("bd","ac"),("c","abd"),("cd","ab"),
           ("d","abc")]) @=? (sort $ uniqueGroups "abcd")]

binaryZip :: [a] -> Int -> ([a],[a])
binaryZip [] _ = ([], [])
binaryZip (i:is) int = add $ binaryZip is rem
    where
        (rem, bit) = divMod int 2
        add (as, bs) = if bit == 1 then (i:as, bs) else (as, i:bs)

binaryZipTest = [
    ("a","bcde") @=? binaryZip "abcde"   1,
    ("abcde","") @=? binaryZip "abcde" 511,
    ("abd","ce") @=? binaryZip "abcde"  11,
    ("","abcde") @=? binaryZip "abcde"   0,
    ("abcde","") @=? binaryZip "abcde" 255]

unitTests = map TestCase $
    uniqueGroupsTest ++
    binaryZipTest ++
    productListTest

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = if isJust items
        then take (fromJust items) $ drop start problem0032
        else drop start problem0032
    printf "List of Pandigital products:\n" 
    mapM_ (\(Triplet a b c) -> printf "%d * %d = %d\n" a b c)  answer
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
