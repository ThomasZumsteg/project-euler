{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Set as Set
import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.List (sort, tails)

import Common (exec, EulerArg, euler_main, primes, isPrime)

-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
-- Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

problem0060 :: Int -> Int -> [Set.Set Integer]
problem0060 n m = primeSets

primePair :: Integer -> Integer -> Bool
primePair m n = isPrime (read $ shows m $ show n) &&
                isPrime (read $ shows n $ show m)

primeSets :: [Set.Set Integer]
primeSets = do 
    a <- primes
    let w = filter (primePair a) $ dropWhile (<= a) primes
    b <- w
    let x = filter (primePair b) $ dropWhile (<= b) primes
    c <- x
    let y = filter (primePair c) $ dropWhile (<= c) primes
    d <- y
    let z = filter (primePair d) $ dropWhile (<= d) primes
    e <- z
    return $ Set.fromList [a,b,c,d,e]

property :: (Show a) => [a] -> Bool
property [] = True
property xs = isPrime $ read $ concatMap show xs

propertyTest = [
    True @=? property [3,7],
    True @=? property [7,3],
    False @=? property [11,7]
    ]

sets :: (Num a, Ord a) => Int -> [a] -> [Set.Set a]
sets _ [] = []
sets 1 xs = map Set.singleton xs
sets size (x:xs) = listMerge (Set.foldl (+) 0) [with_x, without_x]
    where
        without_x = sets size xs
        with_x = map (Set.insert x) $ sets (size - 1) xs

makeSetList = map Set.fromList 

setsTest = [
    (makeSetList [[1,2]]) @=? sets 2 [1,2],
    (makeSetList [[1,2],[1,3],[2,3]]) @=? sets 2 [1,2,3],
    (makeSetList [[1,2],[1,3],[2,3],[1,4],[2,4],[3,4]]) @=? sets 2 [1..4],
    (makeSetList [[1,2],[1,3],[2,3],[1,4],[2,4],[1,5],
        [3,4],[2,5],[3,5],[4,5]]) @=? sets 2 [1..5],
    (makeSetList [[i] | i <- [1..5]]) @=? (take 5 $ sets 1 [1..]),
    (makeSetList [[1,2],[1,3],[2,3],[1,4],[2,4],[1,5]]) 
        @=? (take 6 $ sets 2 [1..])
    ]

listMerge :: (Ord b) => (a -> b) -> [[a]] -> [a]
listMerge _ [] = []
listMerge f (x:xs) | null x = listMerge f xs
listMerge f ((x:xs):ys) = x : listMerge f (if null xs then ys else ys')
    where
        ys' = insertBy (f . head) xs ys

listMergeTest = [
    [] @=? listMerge id ([]::[[Integer]]),
    [2] @=? listMerge id [[],[2]],
    [1,2] @=? listMerge id [[1],[2]],
    [1,2,3] @=? listMerge id [[i] | i <- [1,2,3]],
    [1,2,3] @=? (take 3 $ listMerge id [[i] | i <- [1..]])
    ]

insertBy :: (Ord b) => (a -> b) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy f x ys@(y:ys') = if f x < f y 
    then x:ys 
    else y:(insertBy f x ys')

insertByTest = [
    [0] @=? insertBy id 0 [],
    [0,1] @=? insertBy id 0 [1],
    [1,2,3] @=? insertBy id 2 [1,3],
    [1,2,3,5] @=? (take 4 $ insertBy id 2 [1,3..]),
    [[1],[2],[3]] @=? insertBy head [2] [[i] | i <- [1,3]],
    [[1],[2],[3]] @=? (take 3 $ insertBy head [2] [[i] | i <- [1,3..]]),
    [[2]] @=? insertBy head [2] [],
    [[1],[2]] @=? insertBy head [2] [[1]],
    [[1],[2],[3]] @=? insertBy head [2] [[1],[3]],
    [[1],[2],[3]] @=? (take 3 $ insertBy head [2] [[i] | i <- [1,3..]])
    ]

merge :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge f xs@(x:xs') ys@(y:ys')
    | f x > f y = y:(merge f xs ys')
    | otherwise = x:(merge f xs' ys)

mergeTest = [
    "" @=? merge id "" "",
    "abc" @=? merge id "abc" "",
    "abc" @=? merge id "" "abc",
    "abc" @=? merge id "b" "ac",
    [1,2,3,4] @=? (take 4 $ merge id [1,3,5] [2,4,6]),
    [1,2,3,4] @=? (take 4 $ merge id [1,3..] [2,4..]),
    [[1],[2],[3],[4]] @=? (take 4 $ 
        merge sum [[i] | i <- [3,6..]] $ 
        merge sum [[j] | j <- [1,4..]] [[k] | k <- [2,5..]])
    ]

orderings :: (Ord a) => Int -> Set.Set a -> Set.Set [a]
orderings n s 
    | Set.null s = Set.empty
    | n == 1 = Set.map (:[]) s
    | otherwise = Set.unions [addHead s x | x <- Set.toList s]
    where
        addHead s' x' = Set.map (x':) (orderings (n-1) $ Set.delete x' s')
        

orderingsTest = [
    (Set.fromList ["1","2","3"]) @=? orderings 1 (Set.fromList "123"),
    (Set.fromList ["123","132","213","231","312","321"]) @=? orderings 3 (Set.fromList "123"),
    (Set.fromList ["13","31","12","21","23","32"]) @=? orderings 2 (Set.fromList "123")
    ]

unitTests = map TestCase $ 
    propertyTest ++
    setsTest ++
    mergeTest ++
    insertByTest ++
    listMergeTest ++
    orderingsTest

data Arg = Euler | AdHoc { set_size::Int, cat_size::Int } | UnitTest
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = sum $ Set.toList $ head $ problem0060 5 2
        printf "Answer: %s\n" (show answer)
    exec AdHoc{..} = do
        let answer = problem0060 set_size cat_size
        mapM_ (printf "%s\n" . show) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, AdHoc {set_size=4, cat_size=2}, UnitTest]
