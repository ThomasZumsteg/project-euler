{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Set as Set
import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.List (subsequences)

import Common (exec, EulerArg, euler_main, primes, isPrime)

-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
-- Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

problem0060 :: Int -> [Set.Set Integer]
problem0060 n = filter ((==n) . length) primeSets

concatPairs :: Set.Set Integer -> Set.Set Integer
concatPairs = Set.map read' . combinations . show'
    where
        read' (a, b) = read (a ++ b)
        show' = Set.map show

concatPairsTest = [ 
    (Set.fromList [12,21]) @=? (concatPairs $ Set.fromList [1,2]),
    (Set.fromList [12,21,13,31,23,32]) @=? (concatPairs $ Set.fromList [1,2,3])]

combinations :: (Ord a) => Set.Set a -> Set.Set (a, a)
combinations s = Set.fromList [(n, m) | n <- ls, m <- ls, m /= n]
    where
        ls = Set.toList s

combinationsTest = [
    (Set.fromList []) @=? (combinations $ Set.fromList ""),
    (Set.fromList [('a','b'), ('b','a')]) @=? (combinations $ Set.fromList "ab")]

primeSets :: [Set.Set Integer]
primeSets = map Set.fromList [ [l] | l <- [0..]]

primeSetsTest = [
    Set.fromList [] @=? primeSets !! 0,
    Set.fromList [2] @=? primeSets !! 1,
    Set.fromList [3] @=? primeSets !! 2,
    Set.fromList [2,3] @=? primeSets !! 3,
    Set.fromList [5] @=? primeSets !! 4,
    Set.fromList [2,5] @=? primeSets !! 5,
    Set.fromList [7] @=? primeSets !! 6,
    Set.fromList [3,5] @=? primeSets !! 7,
    Set.fromList [2,3,5] @=? primeSets !! 8]

nCombinations :: Int -> [a] -> [[a]]
nCombinations 0 _ = [[]]
nCombinations _ [] = []
nCombinations i (x:xs) = (map (x:) $ nCombinations (i-1) xs) ++ (nCombinations i xs)

nCombinationsTest = [
    ["ab","ac","bc"] @=? nCombinations 2 "abc",
    ["ab"] @=? nCombinations 2 "ab",
    ["a", "b"] @=? nCombinations 1 "ab",
    ["a"] @=? nCombinations 1 "a",
    [] @=? nCombinations 1 ""]

unitTests = map TestCase $
    -- concatPairsTest ++
    -- combinationsTest ++
    nCombinationsTest
    -- primeSetsTest

data Arg = Euler | AdHoc { limit::Double } | UnitTest
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = sum $ Set.toList $ head $ problem0060 5
        printf "Answer: %s\n" answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, AdHoc {limit = 0.15}, UnitTest]
