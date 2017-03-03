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
        deriving Show

instance (Num a, Ord a) => Ord (ListHeap a) where
    compare Nil Nil = EQ
    compare Nil _ = LT
    compare _ Nil = GT
    compare (List l1 _ _) (List l2 _ _) = compare (sum l1) (sum l2)

instance (Num a, Ord a) => Eq (ListHeap a) where
    h1 == h2 = compare h1 h2 == EQ

listHeapOrdTest = [
    EQ @=? compare (List [] Nil Nil) (List [] Nil Nil),
    EQ @=? compare (List [1] Nil Nil) (List [1] Nil Nil),
    EQ @=? compare (List [2] Nil Nil) (List [1,1] Nil Nil),
    GT @=? compare (List [0] Nil Nil) Nil,
    LT @=? compare (List [0] Nil Nil) (List [1] Nil Nil)
    ]

singleton :: [a] -> ListHeap a
singleton v = List v Nil Nil

remove :: (Ord a, Num a) => ListHeap a -> Maybe ([a], ListHeap a)
remove Nil = Nothing
remove (List v l r) = Just (v, merge l r)

merge :: (Ord a, Num a) => ListHeap a -> ListHeap a -> ListHeap a
merge Nil Nil = Nil
merge h1 Nil = h1
merge Nil h2 = h2
merge h1@(List v l1 r1) h2
    | h1 > h2 = merge h2 h1
    | otherwise = List v l1 (merge r1 h2)

fromList :: (Num a, Ord a) => [[a]] -> ListHeap a
fromList [] = Nil
fromList (x:xs) = merge (singleton x) $ fromList xs

listHeapTest = [
    (List [] Nil Nil) @=? singleton [],
    (List [1] Nil Nil) @=? singleton [1],
    Nothing @=? remove Nil,
    Just ([1], Nil) @=? remove (List [1] Nil Nil)
    ]
    

problem0060 :: Int -> [Set.Set Integer]
problem0060 = error "Not Implemented"

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
    listHeapOrdTest ++
    listHeapTest

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
