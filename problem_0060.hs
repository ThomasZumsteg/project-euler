{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Set as Set
import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main, primes, isPrime)

-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
-- Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

problem0060 :: Int -> [Set.Set Integer]
problem0060 n = filter ((==n) . Set.size) primeSets
    where
        isPrimeSet = all isPrime . concatPairs
        primeSets = error "Not Implemented"

concatPairs :: Set.Set Integer -> Set.Set Integer
concatPairs = Set.map read' . combinations . show'
    where
        read' (a, b) = read (a ++ b)
        show' = Set.map show

concatPairsTest = [ 
    (Set.fromList [12,21]) @=? (concatPairs $ Set.fromList [1,2]),
    (Set.fromList [12,21,13,31,23,32]) @=? (concatPairs $ Set.fromList [1,2,3])]

combinations :: Set.Set a -> Set.Set (a, a)
combinations = error "Not Implemented"

combinationsTest = [
    (Set.fromList [('a','b'), ('b','a')]) @=? (combinations $ Set.fromList "ab")]

unitTests = map TestCase $
    concatPairsTest ++
    combinationsTest

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
