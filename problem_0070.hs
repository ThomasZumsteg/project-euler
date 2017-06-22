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
        phi = (!!) foldPhi
        items = zip [lower..upper] $ map phi [lower..upper]
        permutations (a, b) = (sort $ show a) == (sort $ show b)
        aOverb (n_a, phi_a) (n_b, phi_b) = compare (n_a * phi_b) (n_b * phi_a) 

adhoc :: Arg -> [AdHocReturn]
adhoc (AdHoc start stop) = [AdHocReturn n p | 
    n <- [start..stop],
    let p = factorPhi $ primeFactors n]

foldPhi :: (Integral a) => [a]
foldPhi = 0 : 0 : foldPhiWorker id [2..]

foldPhiTest = [
    (map (length . slowPhi) [0..10]) @=? (take 11 foldPhi)
    ]

foldPhiWorker :: (Integral a) => (a -> a) -> [a] -> [a]
foldPhiWorker _ [] = []
foldPhiWorker update (x:xs) = (if x == x' then (x' - 1) else x') : foldPhiWorker update' xs
    where
        x' = update x
        update' n
            | x' /= x = update n
            | 0 == mod n x = ((*(x-1)) . flip div x) $ update n
            | otherwise = update n

foldPhiWorkerTest = [
    [] @=? foldPhiWorker id [],
    [1] @=? foldPhiWorker id [2],
    [1,2] @=? foldPhiWorker id [2,3],
    [1,2,2] @=? foldPhiWorker id [2,3,4],
    [1,2,2,4,2,6,4,6,4] @=? foldPhiWorker id [2..10]
    ]

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

fastPhi :: [Int]
fastPhi = 0 : 0 : (fastPhiWorker 2 [2..])

fastPhiTest = [ 
    (length $ slowPhi 1) @=? fastPhi !! 1,
    (length $ slowPhi 2) @=? fastPhi !! 2,
    (length $ slowPhi 3) @=? fastPhi !! 3,
    (length $ slowPhi 4) @=? fastPhi !! 4,
    (length $ slowPhi 5) @=? fastPhi !! 5,
    (length $ slowPhi 6) @=? fastPhi !! 6,
    (length $ slowPhi 7) @=? fastPhi !! 7,
    (length $ slowPhi 10) @=? fastPhi !! 10,
    (length $ slowPhi 100) @=? fastPhi !! 100
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
    foldPhiWorkerTest ++
    foldPhiTest

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
