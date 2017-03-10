{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Prelude hiding (head, tail)

class Heap h where
    singleton :: a -> h a
    empty :: (Eq a) => h a -> Bool
    head :: h a -> a
    tail :: (Ord a) => h a -> h a
    merge :: (Ord a) => h a -> h a -> h a
    mergeList :: (Ord a) => [a] -> h a
    mergeList (v:[]) = singleton v
    mergeList (v:vs) = merge (mergeList vs) (singleton v)
    identical :: (Ord a) => h a -> h a -> Bool
    identical h1 h2
        | (empty h1) && (empty h2) = True
        | (empty h1) || (empty h2) = False
        | (head h1) /= (head h2) = False
        | otherwise = identical (tail h1) (tail h2)

data BinaryTree a = 
    Empty | 
    Branch {
        value::a,
        depth::Int,
        left::BinaryTree a,
        right::BinaryTree a
    } 

instance (Show a) => Show (BinaryTree a) where
    show Empty = "Empty"
    show (Branch v d l r) = "(Branch " ++ 
        show v ++ " " ++ show d ++ " " ++
        show l ++ " " ++ show r ++ ")"
    

instance (Eq a) => Eq (BinaryTree a) where
    (==) Empty Empty = True
    (==) _ Empty = False
    (==) Empty _ = False
    (==) t1 t2 = (head t1) == (head t2)

instance (Ord a, Eq a) => Ord (BinaryTree a) where
    compare Empty Empty = EQ
    compare Empty _ = GT
    compare _ Empty = LT
    compare (Branch v1 d1 l1 r1) (Branch v2 d2 l2 r2)
        | v1 /= v2  = compare v1 v2
        | d1 /= d2  = compare d1 d2
        | otherwise = compare  (min l2 r2) (min l1 r1)

instance Heap BinaryTree where
    singleton v = Branch v 0 Empty Empty
    empty = (Empty==)
    head = value
    tail t = merge (left t) (right t)
    merge t1 Empty = t1
    merge Empty t2 = t2
    merge t1@(Branch v1 d1 l1 r1) t2@(Branch v2 d2 _ _)
        | t1 > t2 = merge t2 t1
        | empty l1 = Branch v1 (d1 + d2 + 1) t2 r1 
        | empty r1 = Branch v1 (d1 + d2) l1 t2
        | (depth l1) > (depth r1) = Branch v1 (d1 + d2) l1 (merge r1 t2)
        | otherwise = Branch v1 (d1 + d2) (merge l1 t2) r1

oneBranch = (Branch 1 0 Empty Empty)
twoBranch = (Branch 2 0 Empty Empty)
threeBranch = (Branch 3 0 Empty Empty)
oneTwoBranch = (Branch 1 1 twoBranch Empty)
oneTwoThreeBranch = (Branch 3 1 oneBranch twoBranch)

unitTests = map TestCase $ [
    oneBranch @=? singleton 1,
    twoBranch @=? singleton 2,
    False @=? oneBranch == twoBranch,
    True @=? oneBranch == oneBranch,
    True @=? oneBranch < twoBranch,
    True @=? oneBranch == oneTwoBranch,
    False @=? identical oneBranch oneTwoBranch,
    1 @=? head oneBranch,
    2 @=? head twoBranch,
    Empty @=? tail oneBranch,
    twoBranch @=? tail oneTwoBranch,
    oneBranch @=? merge oneBranch Empty,
    oneBranch @=? merge oneBranch twoBranch,
    True @=? empty (Empty::BinaryTree Int),
    False @=? empty oneBranch
    ]

main :: IO ()
main = do
    runTestTT $ TestList unitTests
    print $ show (merge (singleton 3) oneTwoBranch)
    print $ show ((merge (singleton 2) (singleton 1))::BinaryTree Int)
    print $ show ((mergeList [1,2,3])::(BinaryTree Int))
    return ()
