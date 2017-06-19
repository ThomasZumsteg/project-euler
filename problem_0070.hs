{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf, PrintfArg, formatArg, formatString)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main, primes)

import Data.List (minimumBy, sort, group)

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
problem (Problem lower upper) = toInteger $ minimumBy nOverPhi $ 
    filter permutations [lower..upper]
    where
        size = (!!) fastPhi
        permutations n = (sort $ show n) == (sort $ show $ size n)
        nOverPhi a b = compare (a * size b) (b * size a) 

adhoc :: Arg -> [AdHocReturn]
adhoc (AdHoc start stop) = [AdHocReturn n p | 
    n <- [start..stop],
    let p = factorPhi $ primeFactors n]

foldPhi :: [Integer]
foldPhi = -1 : -1 : foldMapl updater [] [2..]

foldMapl :: (s -> a -> (b, s)) -> s -> [a] -> [b]
foldMapl _ _ [] = []
foldMapl update s (x:xs) = x' : foldMapl update s' xs
    where
        (x', s') = update s x

foldMaplTest = [
    [1,2,3] @=? foldMapl (\s a -> (s + a, s + a)) 0 [1,1,1],
    [1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8] @=? foldMapl updater [] [2..20]
    ]

updater :: (Integral a) => [a] -> a -> (a, [a])
updater primes v 
    | v == v' = (v - 1, v:primes)
    | otherwise = (v', primes)
    where
        v' = foldl worker v primes
            where
                worker val prime = if 0 == mod v prime
                    then (div val prime) * (prime - 1)
                    else val

updaterTest = [
    (2, [3,2]) @=? updater [2] 3,
    (2, [3,2]) @=? updater [3,2] 4,
    (4, [5,3,2]) @=? updater [3,2] 5,
    (2, [5,3,2]) @=? updater [5,3,2] 6,
    (6, [7,5,3,2]) @=? updater [5,3,2] 7,
    (4, [7,5,3,2]) @=? updater [7,5,3,2] 8,
    (6, [2,3,5,7]) @=? updater [2,3,5,7] 9,
    (6, [7,5,3,2]) @=? updater [7,5,3,2] 9
    ]

phi :: Integer -> [Integer]
phi 0 = []
phi 1 = []
phi n = 1 : [i | i <- [2..(n-1)], 1 == gcd i n]

phiTest = [
    [] @=? phi 1,
    [1] @=? phi 2,
    [1,2] @=? phi 3,
    [1,3] @=? phi 4,
    [1,2,3,4] @=? phi 5,
    [1,5] @=? phi 6,
    [1,2,3,4,5,6] @=? phi 7,
    [1,3,5,7] @=? phi 8,
    [1,2,4,5,7,8] @=? phi 9,
    [1,3,7,9] @=? phi 10
    ]

fastPhi :: [Int]
fastPhi = 0 : 0 : (fastPhiWorker 2 [2..])

fastPhiTest = [ 
    (length $ phi 1) @=? fastPhi !! 1,
    (length $ phi 2) @=? fastPhi !! 2,
    (length $ phi 3) @=? fastPhi !! 3,
    (length $ phi 4) @=? fastPhi !! 4,
    (length $ phi 5) @=? fastPhi !! 5,
    (length $ phi 6) @=? fastPhi !! 6,
    (length $ phi 7) @=? fastPhi !! 7,
    (length $ phi 10) @=? fastPhi !! 10,
    (length $ phi 100) @=? fastPhi !! 100
    ]   

fastPhiWorker :: Int -> [Int] -> [Int]
fastPhiWorker i (x:xs)
    | i == x = (x - 1) : fastPhiWorker (i+1) xs'
    | otherwise = x : fastPhiWorker (i+1) xs
        where
            xs' = updateNth update (x-1) xs
            update n = (x-1) * div n x

updateNth :: (a -> a) -> Int -> [a] -> [a]
updateNth update i xs = worker i xs
    where
        worker _ [] = []
        worker 0 (x:xs) = update x : worker i xs
        worker n (x:xs) = x : worker (n-1) xs

factorPhi :: [Integer] -> Integer
factorPhi primeFactors = foldl calculatePhi 1 $ group $ sort primeFactors
    where
        calculatePhi total ps@(p:_) = total * (p - 1) * (toInteger $ length ps)

factorPhiTest = [
    1 @=? factorPhi [2],
    2 @=? factorPhi [3],
    4 @=? factorPhi [5],
    2 * 4 @=? factorPhi [3,5]
    ]

primeFactors :: Integer -> [Integer]
primeFactors n = worker n primes
    where
        worker n ps@(p:ps')
            | n == 1 = []   
            | r == 0 = p : worker q ps
            | otherwise = worker n ps'
                where
                    (q, r) = divMod n p

primeFactorsTest = [
    [2,5] @=? primeFactors 10,
    [2,2,2] @=? primeFactors 8
    ]

primeFactorsList :: [[Integer]]
primeFactorsList = []  : [] : (worker 2 $ repeat [])
    where
        worker i (x:xs) 
            | x == [] = [i] : (worker (i+1) $ mapNth i (i:) xs)
            | prod == i = x : (worker (i+1) xs)
            | otherwise = ((primeFactorsList !! rem) ++ x) : (worker (i+1) xs)
                where
                    prod = foldl1 (*) x
                    rem = fromInteger $ div i prod

primeFactorsListTest = [
    [] @=? primeFactorsList !! 0,
    [] @=? primeFactorsList !! 1,
    [2] @=? primeFactorsList !! 2,
    [3] @=? primeFactorsList !! 3,
    [2,2] @=? primeFactorsList !! 4,
    [5] @=? primeFactorsList !! 5,
    [3,2] @=? primeFactorsList !! 6,
    [7] @=? primeFactorsList !! 7,
    [2,2,2] @=? primeFactorsList !! 8,
    [3,3] @=? primeFactorsList !! 9,
    [5,2] @=? primeFactorsList !! 10
    ]

mapNth :: Integer -> (a -> a) -> [a] -> [a]
mapNth n f xs = worker 2 xs
    where
        worker _ [] = []
        worker i (x:xs) = x' : worker i' xs
            where 
                x' = if i == 1 then f x else x
                i' = if i < n then i + 1 else 1

mapNthTest = [
    [1,2,1,2] @=? mapNth 2 (+1) [1,1,1,1],
    [1,1,3,1] @=? mapNth 3 (+2) [1,1,1,1],
    [[],[1],[],[1]] @=? mapNth 2 (1:) [[],[],[],[]]
    ]

unitTests = map TestCase $
    phiTest ++
    mapNthTest ++
    primeFactorsListTest ++
    factorPhiTest ++
    primeFactorsTest ++
    fastPhiTest ++
    foldMaplTest ++
    updaterTest

data Arg = Euler | UnitTest |
    AdHoc { adhocLowerLimit::Integer, adhocUpperLimit::Integer } 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = problem (Problem { pLowerLimit = 1, pUpperLimit = 10^7 })
        printf "Answer %d\n" answer
    exec args@(AdHoc {..}) = do
        let answers = adhoc args
        mapM_ (printf "%v\n") answers
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { adhocLowerLimit = 1, adhocUpperLimit = 100 }]
