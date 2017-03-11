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
problem0060 n = error "Not Implemented"

property :: Integer -> Integer -> Bool
property a b = isPrime $ read (show a ++ show b)

propertyTest = [
    True @=? property 3 7,
    True @=? property 7 3,
    False @=? property 11 7
    ]

nLenSets :: Int -> [a] -> [[a]]
nLenSets 0 _ = [[]]
nLenSets _ [] = []
nLenSets n (x:xs) = with_x ++ without_x
    where
        with_x = map (x:) $ nLenSets (n-1) xs
        without_x = nLenSets n xs

deepSort = sort . map sort

nLenSetsTest = [
    [[1,2],[1,3],[2,3]] @=? (deepSort $ nLenSets 2 [1,3,2]),
    [[1,2],[1,3],[2,3]] @=? (deepSort $ nLenSets 2 [1,2,3]),
    [[1,2]] @=? (deepSort $ nLenSets 2 [1,2]),
    [[1],[2]] @=? (deepSort $ nLenSets 1 [1,2])
    ]

unitTests = map TestCase $ 
    propertyTest ++
    nLenSetsTest

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
