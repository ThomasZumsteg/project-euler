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
    singleton :: a -> h
    remove :: h -> Maybe (a, h)

data ListHeap a = Nil | Value a | List [ListHeap a]

problem0060 :: Int -> [[Integer]]
problem0060 = filter (all property . pairs) . toList . setsOfLength primes

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, y) | y <- xs] ++ pairs xs

pairsTest = [
    [] @=? pairs "",
    [] @=? pairs [1],
    [(1,2)] @=? pairs [1,2],
    [(1,2),(1,3),(2,3)] @=? pairs [1,2,3]
    ]

property :: (Integer, Integer) -> Bool
property (p, q) = pqIsPrime && qpIsPrime
    where
        p' = show p
        q' = show q
        qpIsPrime = isPrime $ read $ p' ++ q'
        pqIsPrime = isPrime $ read $ q' ++ p'

propertyTest = [
    True @=? property (3, 7),
    True @=? property (3, 109),
    True @=? property (109, 3),
    False @=? property (101, 3)
    ]

combinations :: Int -> [a] -> ListHeap a
combinations = error "Not Implemeneted"

combinationsTest = [
    ]

toList :: ListHeap a -> [a]
toList = error "Not Implemented"

toListTest = [
    ]

setsOfLength :: [a] -> Int -> ListHeap [a]
setsOfLength = error "Not Implemented"

setsOfLengthTest = [
    ]

unitTests = map TestCase $
    setsOfLengthTest ++
    toListTest ++
    combinationsTest ++
    propertyTest ++ 
    pairsTest

data Arg = Euler | AdHoc { limit::Double } | UnitTest
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = head $ problem0060 5
        printf "Answer: %s\n" (show answer)
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, AdHoc {limit = 0.15}, UnitTest]
