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

class Heap h where
    singleton :: a -> h a
    remove :: h a -> Maybe (a, h a)
    merge :: h a -> h a -> h a
    insert :: h a -> a -> h a
    insert h v = merge h (singleton v)
    toList :: h a -> [a]
    toList heap = case remove heap of
        Just (x, heap') -> x : toList heap'
        otherwise -> []
    fromList :: [a] -> h a
    fromList (x:[]) = singleton x
    fromList (x:xs) = merge (singleton x) (fromList xs)

data BHeap a = Empty | Leaf { 
    value :: a, 
    _nodes :: Int,
    left :: BHeap a,
    right :: BHeap a }
    deriving Eq

instance (Show a) => Show (BHeap a) where 
    show Empty = "Empty"
    show (Leaf v d lh rh) = "(Leaf " ++ show v ++ " " ++ show d ++ 
        " " ++ show lh ++ " " ++ show rh ++ ")"

instance (Ord a) => Heap (BHeap a) where
    remove Empty = Nothing 
    remove (Leaf v _ l r) = Just (v, merge l r)
    singleton v = Leaf v 1 Empty Empty
    merge = bMerge

bMerge :: (Ord a) => BHeap a -> BHeap a -> BHeap a
bMerge Empty h2 = h2
bMerge h1 Empty = h1
bMerge h1@(Leaf v1 n1 l1 r1) h2@(Leaf v2 n2 l2 r2) 
    | v2 < v1  = bMerge (Leaf v2 n1 l1 r1) (Leaf v1 n2 l2 r2)
    | (v2 == v1 && n1 < n2) = bMerge h2 h1
    | nodes l1 > nodes r1 = h' l1 (bMerge r1 h2)
    | otherwise = h' (bMerge l1 h2) r1
        where
            h' = Leaf v1 (n1 + n2)
            nodes Empty = 0
            nodes h = _nodes h


bMergeTest = [
    (Leaf 1 2 (Leaf 2 1 Empty Empty) Empty) @=?  bMerge 
    (Leaf 2 1 Empty Empty) 
    (Leaf 1 1 Empty Empty)]

insertTest = [
    (Leaf 1 3 (Leaf 3 1 Empty Empty) (Leaf 2 1 Empty Empty)) @=?  
        insert (Leaf 2 2 (Leaf 3 1 Empty Empty) Empty) 1,
    (Leaf 1 2 (Leaf 3 1 Empty Empty) Empty) @=? insert (Leaf 3 1 Empty Empty) 1,
    [1] @=? (toList $ insert Empty 1),
    [1, 2] @=? (toList $ insert (Leaf 2 1 Empty Empty) 1),
    [1, 2] @=? (toList $ insert (Leaf 1 1 Empty Empty) 2),
    [2] @=? (toList $ insert Empty 2)]

fromListTest = [
    [1,2,3] @=? (toList $ fromList [3,2,1]::[Integer]),
    ([1,2,3]::Integer) @=? (toList $ fromList [1,3,2]),
    (Leaf 1 3 (Leaf 2 1 Empty Empty) (Leaf 3 1 Empty Empty)) @=? fromList [1,2,3]]

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
    [] @=? (take 10 $ nCombinations 2 [1..]),
    ["ab","ac","bc"] @=? nCombinations 2 "abc",
    ["ab"] @=? nCombinations 2 "ab",
    ["a", "b"] @=? nCombinations 1 "ab",
    ["a"] @=? nCombinations 1 "a",
    [] @=? nCombinations 1 ""]

unitTests = map TestCase $
    bMergeTest ++
    fromListTest

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
