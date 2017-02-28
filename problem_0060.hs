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
    remove :: (Ord a) => h a -> Maybe (a, h a)
    merge :: (Ord a) => h a -> h a -> h a

    insert :: (Ord a) => h a -> a -> h a
    insert h v = merge h (singleton v)

    toList :: (Ord a) => h a -> [a]
    toList heap = case remove heap of
        Just (x, heap') -> x : toList heap'
        otherwise -> []

    fromList :: (Ord a) => [a] -> h a
    fromList (x:[]) = singleton x
    fromList (x:xs) = merge (singleton x) (fromList xs)

data BHeap a = Empty | Leaf {
    value :: a,
    _nodes :: Integer,
    left  :: BHeap a,
    right :: BHeap a
} deriving Eq

instance (Show a) => Show (BHeap a) where
    show Empty = "Empty"
    show (Leaf v n l r) = "(" ++ show v ++ " " ++ show n ++ " " ++
        show l ++ " " ++ show r ++ ")"

instance Heap BHeap where
    singleton v = Leaf v 1 Empty Empty
    remove Empty = Nothing
    remove (Leaf v _ l r) = Just (v, merge l r)
    merge Empty h2 = h2
    merge h1 Empty = h1
    merge h1@(Leaf v1 n1 l1 r1) h2@(Leaf v2 n2 _ _)
        | v2 < v1 = merge h2 h1
        | v2 == v1 && n1 < n2 = merge h2 h1
        | nodes r1 < nodes l1 = Leaf v1 (n1 + n2) r1 (merge l1 h2)
        | otherwise = Leaf v1 (n1 + n2) (merge r1 h2) l1
        where
            nodes Empty = 0
            nodes h = _nodes h

bHeapTest = [
    (Leaf 1 1 Empty Empty) @=? singleton 1,
    (Leaf 0 1 Empty Empty) @=? singleton 0,
    "(1 1 Empty Empty)" @=? (show $ (singleton 1::BHeap Integer)),
    "(0 1 Empty Empty)" @=? (show $ (singleton 0::BHeap Integer)),
    (Leaf 0 2 (Leaf 1 1 Empty Empty) Empty) @=? merge (singleton 1) (singleton 0),   
    (Leaf 0 2 (Leaf 1 1 Empty Empty) Empty) @=? merge (singleton 0) (singleton 1),
    (Leaf 0 2 (Leaf 1 1 Empty Empty) Empty) @=? insert (singleton 0) 1,
    (Leaf 0 2 (Leaf 1 1 Empty Empty) Empty) @=? insert (singleton 1) 0]

data LeftHeap a = 
    Nil | 
    LeftLeaf a (LeftHeap a) | 
    RightLeaf a (LeftHeap a) (LeftHeap a) 
        deriving (Eq, Show)

instance (Ord a) => Ord (LeftHeap a) where
    compare Nil Nil = EQ
    compare Nil _ = LT
    compare _ Nil = GT
    compare (LeftLeaf v1 l1) (LeftLeaf v2 l2) 
        | v1 == v2  = compare l1 l2
        | otherwise = compare v1 v2
    compare h1@(RightLeaf v1 l1 r1) h2@(RightLeaf v2 l2 r2)
        | v1 /= v2 = compare v1 v2
        | otherwise = compare (min l1 r1) (min l2 r2)

instance Heap LeftHeap where
    singleton v = LeftLeaf v Nil
    remove Nil = Nothing
    remove (LeftLeaf v l) = Just (v, l)
    remove (RightLeaf v l r) = Just (v, merge l r)
    merge h1 Nil = h1
    merge Nil h2 = h2

leftHeapTest = [
    (LeftLeaf 1 Nil) @=? singleton 1,
    (LeftLeaf 0 Nil) @=? singleton 0,
    Nothing @=? remove (Nil::(LeftHeap Int)),
    Just (0, (LeftLeaf 1 Nil)) @=? 
        remove ((LeftLeaf 0 (LeftLeaf 1 Nil))::(LeftHeap Int)),
    Just (0, (LeftLeaf 1 (LeftLeaf 2 Nil))) @=? 
        remove ((LeftLeaf 0 (LeftLeaf 1 (LeftLeaf 2 Nil)))::(LeftHeap Int)),
    Just (0,  (LeftLeaf 1 (LeftLeaf 2 Nil))) @=?
        remove ((LeftLeaf 0 (LeftLeaf 1 (LeftLeaf 2 Nil)))::(LeftHeap Int)),
    [0,1,2,3] @=? (take 4 $ toList $ (fromList ([0..])::(LeftHeap Int))),
    [0,1,2,3] @=? (toList $ (fromList ([0,1,2,3])::(LeftHeap Int))),
    True @=? ((LeftLeaf 0 Nil) < (LeftLeaf 1 Nil))
    ]

problem0060 :: Int -> [Set.Set Integer]
problem0060 = error "Not Implemented"

unitTests = map TestCase $
    bHeapTest ++
    leftHeapTest

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
