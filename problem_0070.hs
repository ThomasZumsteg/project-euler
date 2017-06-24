{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf, PrintfArg, formatArg, formatString)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main, primes)

import Data.List (minimumBy, sort, group, foldl')

-- Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
-- The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
-- Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
-- Find the value of n, 1 < n < 10⁷, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum

data Problem = Problem { pLowerLimit :: Int, pUpperLimit :: Int }
data AdHocReturn = AdHocReturn { num :: Integer, ah_phi :: Integer }

instance PrintfArg AdHocReturn where
    formatArg (AdHocReturn { num = n, ah_phi = ah_phi }) = formatString $
            ((printf "%7d:%7d - %4.2f" n len frac)::String)
        where
            len = ah_phi
            frac = ((fromInteger n) / (fromInteger len))::Double

problem :: Problem -> Integer
problem (Problem lower upper) = toInteger $ fst $ minimumBy aOverb $ filter permutations items
    where
        phi = length . slowPhi
        items = zip [lower..upper] $ map phi [lower..upper]
        permutations (a, b) = (sort $ show a) == (sort $ show b)
        aOverb (n_a, phi_a) (n_b, phi_b) = compare (n_a * phi_b) (n_b * phi_a) 

adhoc :: Arg -> [AdHocReturn]
adhoc (AdHoc start stop) = [AdHocReturn n p | 
    phi <- [stop..start],
    let p = primeFactors]

slowPhi :: (Integral a) => a -> [a]
slowPhi 0 = []
slowPhi 1 = []
slowPhi n = 1 : [i | i <- [2..(n-1)], 1 == gcd i n]

phiTest = [
    [] @=? slowPhi 1,
    [1] @=? slowPhi 2,
    [1,2] @=? slowPhi 3,
    [1,3] @=? slowPhi 4,
    [1,2,3,4] @=? slowPhi 5,
    [1,5] @=? slowPhi 6,
    [1,2,3,4,5,6] @=? slowPhi 7,
    [1,3,5,7] @=? slowPhi 8,
    [1,2,4,5,7,8] @=? slowPhi 9,
    [1,3,7,9] @=? slowPhi 10
    ]

unitTests = map TestCase $
    phiTest

data Arg = Euler | UnitTest |
    AdHoc { adhocLowerLimit::Integer, adhocUpperLimit::Integer } 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = problem (Problem { pLowerLimit = 1, pUpperLimit = 10^4 })
        printf "Answer %d\n" answer
    exec args@(AdHoc {..}) = do
        let answers = adhoc args
        mapM_ (printf "%v\n") answers
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { adhocLowerLimit = 1, adhocUpperLimit = 100 }]
