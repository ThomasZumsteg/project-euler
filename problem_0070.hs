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
    n <- [stop..start],
    let p = toInteger $ length $ slowPhi n]

phiFactors :: Integer -> [[Integer]] 
phiFactors limit = primes'
    where
        primes' = reverse $ map (:[]) $ takeWhile (<limit) primes        

phiFactorsTest = [
    -- (sort $ map (toInteger . length . slowPhi) [1..10]) @=? 
    --     (map (foldl (*) 1) $ phiFactors 10),
    [29] @=? phiFactors 30 !! 0,
    [23] @=? phiFactors 30 !! 1,
    [19] @=? phiFactors 30 !! 2,
    [17] @=? phiFactors 30 !! 3,
    [13] @=? phiFactors 30 !! 4,
    [3] @=? phiFactors 30 !! 10,
    [2,11] @=? phiFactors 30 !! 20,
    [] @=? phiFactors 30 !! 30
    ]


-- Phi(n) must be less then n
--
-- [   29] = (29,1.0357142857142858)
-- [   23] = (23,1.0454545454545454)
-- [   19] = (19,1.0555555555555556)
-- [   17] = (17,1.0625)
-- [   13] = (13,1.0833333333333333)
-- [   11] = (11,1.1)
-- [    7] = (7,1.1666666666666667)
-- [    5] = (5,1.25)
-- [ 5, 5] = (25,1.25)
-- [    3] = (3,1.5)
-- [ 3, 3] = (9,1.5)
-- [3,3,3] = (27,1.5)
-- [ 3, 7] = (21,1.75)
-- [ 3, 5] = (15,1.875)
-- [    2] = (2,2.0)
-- [ 2, 2] = (4,2.0)
-- [2,2,2] = (8,2.0)
-- [2,2,2,2] = (16,2.0)
-- [ 2,13] = (26,2.1666666666666665)
-- [ 2,11] = (22,2.2)
-- [ 2, 7] = (14,2.3333333333333335)
-- [2,2,7] = (28,2.3333333333333335)
-- [ 2, 5] = (10,2.5)
-- [2,2,5] = (20,2.5)
-- [ 2, 3] = (6,3.0)
-- [2,2,3] = (12,3.0)
-- [2,3,3] = (18,3.0)
-- [2,2,3,3] = (24,3.0)
-- [2,3,5] = (30,3.75)
-- (1,Infinity)

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
    phiTest ++
    phiFactorsTest

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
