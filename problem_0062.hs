{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Data.List (sort)
import Data.Maybe (isJust)
import qualified Data.Map as Map

import Common (exec, EulerArg, euler_main)

-- The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
-- Find the smallest cube for which exactly five permutations of its digits are cube.

problem0062 :: Integer -> Int -> [[Integer]]
problem0062 exp perms = error "Not Implemented"

cubePermutations :: Int -> Map.Map String [String]
cubePermutations numDigits = Map.fromListWith (++) 
    [(sort digits, [digits]) | 
        digits <- takeWhile ((numDigits==) . length) $ 
            dropWhile ((<numDigits) . length) $ 
            map (show . cube) [1..]]
    where
        cube = (flip (^) 3)

-- Needs to be lazy
cubePermutationsTest = [ 
    ["1"] @=? (Map.!) (cubePermutations 1) "1",
    ["27"] @=? (Map.!) (cubePermutations 2) "27",
    ["64"] @=? (Map.!) (cubePermutations 2) "46",
    ["66430125","56623104","41063625"] @=? (Map.!) (cubePermutations 8) "01234566",
    ["66430125","56623104","41063625"] @=? (Map.!) (cubePermutations 10) "01234566"
    ]

-- n^n = num
powers :: Integer -> [Integer]
powers n = filter (error "not Implemented") [2..stop]
    where
        stop = floor $ (log $ fromIntegral n) / (log 2)

test :: Integer -> Integer -> (Integer -> Ordering)
test number base exp = compare (base ^ exp) number

powersTest = [
    [2] @=? powers 4,
    [2,3] @=? powers 64, 
    [] @=? powers 3,
    [2,3,4] @=? powers 4096
    ]

binarySearch :: (a -> Ordering) -> [a] -> Maybe a
binarySearch _ [] = Nothing
binarySearch test xs = case test q of
    LT -> binarySearch test ps
    GT -> binarySearch test qs
    EQ -> Just q
    where
        (ps, (q:qs)) = splitAt mid xs
        mid = div (length xs) 2 

binarySearchTest = [
    Just 2 @=? binarySearch (compare 2) [1..10],
    Just 5 @=? binarySearch (compare 5) [1..10],
    Nothing @=? binarySearch (compare 11) [1..10],
    Nothing @=? binarySearch (compare 5) [2,4..10],
    Just 10 @=? binarySearch (compare 10) [2,4..10],
    Just 2 @=? binarySearch (compare 2) [2,4..10]
    ]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = concatMap addHead [splitAt (i - 1) xs | i <- [1..(length xs)]]
    where
        addHead (ps,(q:qs)) = map (q:) $ permutations (ps ++ qs)

permutationsTest = [
    [""] @=? permutations "",
    ["1"] @=? permutations "1",
    ["12","21"] @=? permutations "12",
    ["123","132", "213", "231", "312", "321"] @=? permutations "123"
    ]

unitTests = map TestCase $
    permutationsTest ++
    binarySearchTest ++
    cubePermutationsTest

data Arg = Euler | UnitTest |
    AdHoc {exponent::Integer, numPermutations::Int} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = head $ problem0062 3 5
        printf "Answer: %d\n" (sum answer)
    exec AdHoc{..} = do
        let answer = problem0062 exponent numPermutations
        mapM_ (printf "%s\n" . show) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {
    exponent=3, numPermutations=3}]
