{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Set as Set
import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.List (sort)

import Common (exec, EulerArg, euler_main, primes, isPrime)

-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
-- Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

problem0060 :: Int -> Int -> [Set.Set Integer]
problem0060 n m = filter (all property . orderings m) $ sets n primes

property :: (Show a) => [a] -> Bool
property [] = True
property xs = isPrime $ read $ concatMap show xs

propertyTest = [
    True @=? property [3,7],
    True @=? property [7,3],
    False @=? property [11,7]
    ]

sets :: (Ord a) => Int -> [a] -> [Set.Set a]
sets = error "Not Implemented"

makeSetList = map Set.fromList 

setsTest = [
    (makeSetList [[1,2]]) @=? sets 2 [1,2],
    (makeSetList [[1,2],[1,3],[2,3]]) @=? sets 2 [1,2,3]
    ]

orderings :: Int -> Set.Set a -> Set.Set [a]
orderings = error "Not Implemented"

orderingsTest = [
    (Set.fromList ["1","2","3"]) @=? orderings 1 (Set.fromList "123"),
    (Set.fromList ["13","31","12","21","23","32"]) @=? orderings 2 (Set.fromList "123")
    ]

unitTests = map TestCase $ 
    propertyTest ++
    orderingsTest ++
    setsTest

data Arg = Euler | AdHoc { limit::Double } | UnitTest
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = sum $ Set.toList $ head $ problem0060 5 2
        printf "Answer: %s\n" (show answer)
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, AdHoc {limit = 0.15}, UnitTest]
