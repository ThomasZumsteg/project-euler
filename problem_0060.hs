{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Set as Set
import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.List (subsequences)

import Data.Maybe (fromJust)

import Common (exec, EulerArg, euler_main, primes, isPrime)

-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
-- Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

data ListHeap a = 
    Nil | List [a] (ListHeap a) (ListHeap a)
        deriving (Show, Eq)

instance (Ord a, Num a) => Ord (ListHeap a) where
    compare Nil Nil = EQ
    compare _ Nil = LT
    compare Nil _ = GT
    compare (List l1 _ _) (List l2 _ _) = compare (sum l1) (sum l2)

problem0060 :: Int -> [Set.Set Integer]
problem0060 = error "Not Implemented"

buildHeap :: [a] -> [a] -> ListHeap a
buildHeap [] [] = Nil
buildHeap root [] = Nil
buildHeap root (x:xs) = List (x:root) without with
    where
        without = buildHeap root xs 
        with = buildHeap (x:root) xs 

buildHeapTest = [
    (List [1] (List [2] Nil Nil) (List [2,1] Nil Nil)) @=? buildHeap [] [1,2],
    (List [1] Nil Nil) @=? buildHeap [] [1],
    Nil @=? buildHeap [1] [],
    (Nil::ListHeap Int) @=? buildHeap [] []
    ]

merge:: (Ord a, Num a) => ListHeap a -> ListHeap a -> ListHeap a
merge Nil Nil = Nil
merge h1 Nil = h1
merge Nil h2 = h2
merge h1@(List v1 l1 r1) h2@(List v2 _ _)
    | h1 > h2 = merge h2 h1
    | l1 == Nil = List v1 h2 r1
    | r1 == Nil = List v1 l1 h2
    | otherwise = List v1 l1 (merge h2 r1)

mergeTest = [
    Nil @=? merge Nil Nil,
    (List [1] Nil Nil) @=? merge Nil (List [1] Nil Nil),
    (List [1] Nil Nil) @=? merge (List [1] Nil Nil) Nil,
    (List [1] (List [2] Nil Nil) Nil) @=? merge (List [1] Nil Nil) (List [2] Nil Nil)
    ]

toList :: (Ord a, Num a) => ListHeap a -> [[a]]
toList Nil = []
toList (List i l r) = i : (toList $ merge l r)

toListTest = [
    ([[1],[2],[3],[2,1],[3,1],[3,2],[3,2,1]]::[[Integer]]) @=? (toList $ buildHeap [] [1,2,3])
    ]

primeSets :: ListHeap [Integer]
primeSets = error "Not Implemented"

-- primeSetsTest = [
--     [2] @=? (toList primeSets) !! 0,
--     [3] @=? (toList primeSets) !! 1,
--     [3,2] @=? (toList primeSets) !! 2,
--     [5] @=? (toList primeSets) !! 3,
--     [5,2] @=? (toList primeSets) !! 4,
--     [5,3] @=? (toList primeSets) !! 5,
--     [5,3,2] @=? (toList primeSets) !! 6,
--     [7] @=? (toList primeSets) !! 7,
--     [7,2] @=? (toList primeSets) !! 8,
--     [7,3] @=? (toList primeSets) !! 9,
--     [11] @=? (toList primeSets) !! 10
--     ]

unitTests = map TestCase $
    buildHeapTest ++
    mergeTest ++
    toListTest

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
