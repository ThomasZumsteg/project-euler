{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Prelude hiding (head)

class Heap h where
    singleton :: a -> h a
    head :: h a -> a
    tail :: (Ord a) => h a -> h a
    merge :: (Ord a) => h a -> h a -> h a

data BinaryTree a = 
    Empty | 
    Branch {
        value::a,
        depth::Int,
        left::BinaryTree a,
        right::BinaryTree a
    } 

instance (Eq a) => Eq (BinaryTree a) where
    (==) Empty Empty = True
    (==) _ Empty = False
    (==) Empty _ = False
    (==) t1 t2 = (head t1) == (head t2)

instance (Ord a, Eq a) => Ord (BinaryTree a) where
    compare (Branch v1 d1 l1 r1) (Branch v2 d2 l2 r2)
        | v1 /= v2  = compare v1 v2
        | d1 /= d2  = compare d1 d2
        | otherwise = compare (min l1 r1) (min l2 r2)

instance Heap BinaryTree where
    singleton v = Branch v 0 Empty Empty
    head = value
    tail t = merge (left t) (right t)
    merge t1 Empty = t1
    merge t1@(Branch v1 d1 l1 r1) t2@(Branch v2 d2 l2 r2)
        | t1 > t2 = merge t2 t1
        | l1 > r1 = Branch v1 d1 (merge l1 t2) r1
        | otherwise = Branch v1 d1 l1 (merge r1 t2)

unitTests = map TestCase $
    []

main :: IO ()
main = do
    runTestTT $ TestList unitTests
    return ()
