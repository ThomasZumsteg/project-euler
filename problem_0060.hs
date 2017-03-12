{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Set as Set
import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.List (sort)

import Common (exec, EulerArg, euler_main, primes, isPrime)

-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
-- Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

problem0060 :: Int -> [Set.Set Integer]
problem0060 n = Set.toAscList $ nLenSets n primes

property :: Integer -> Integer -> Bool
property a b = isPrime $ read (show a ++ show b)

propertyTest = [
    True @=? property 3 7,
    True @=? property 7 3,
    False @=? property 11 7
    ]

nLenSets :: (Ord a) => Int -> [a] -> Set.Set(Set.Set a)
nLenSets 0 _ = Set.singleton Set.empty
nLenSets _ [] = Set.empty
nLenSets n (x:xs) = Set.union with_x without_x
    where
        with_x = Set.map (Set.insert x) $ nLenSets (n-1) xs
        without_x = nLenSets n xs

fromLists = Set.fromList . map Set.fromList

nLenSetsTest = [
    (fromLists [[1,2],[1,3],[2,3]]) @=? nLenSets 2 [1,3,2],
    (fromLists [[1,2],[1,3],[2,3]]) @=? nLenSets 2 [1,2,3],
    (fromLists [[1,2]]) @=? nLenSets 2 [1,2],
    (fromLists [[1],[2]]) @=? nLenSets 1 [1,2]
    ]

unitTests = map TestCase $ 
    propertyTest ++
    nLenSetsTest

data Arg = Euler | AdHoc { limit::Double } | UnitTest
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = sum $ Set.toList $ head $ problem0060 5
        printf "Answer: %s\n" (show answer)
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, AdHoc {limit = 0.15}, UnitTest]
