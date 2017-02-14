{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs =
    Euler
    | AdHoc { limit::Double }
    | UnitTest
    deriving (Show, Data, Typeable)

-- Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
-- 37 36 35 34 33 32 31
-- 38 17 16 15 14 13 30
-- 39 18  5  4  3 12 29
-- 40 19  6  1  2 11 28
-- 41 20  7  8  9 10 27
-- 42 21 22 23 24 25 26
-- 43 44 45 46 47 48 49
-- It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
-- If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

type State = ((Integer, Integer), Integer, [Integer])

problem0058 :: [(Double, Integer, [Integer])]
problem0058 = map divide $ scanl scanPrimes inital $ drop 1 spiral
    where
        divide (s, l, ns) = (div' s, l, ns)
        div' (a, b) = ((fromInteger a) / (fromInteger b))
        inital = ((1,1), 1, [1])

scanPrimes :: State -> [Integer] -> State
scanPrimes ((n, d), l, _) ns = ((n + nPrimes, d + 4), l + 8, diags)
    where
        diags = diagonals ns
        nPrimes = sum $ map (\n -> if isPrime n then 1 else 0) diags

scanPrimesTest = [
    ((3,4), 8, [3,5,7,9]) @=? scanPrimes ((0,0), 0, [0]) [2..9]]

-- d1_1 = d4_0 + (l1 - 1)
-- d2_1 = d1_1 + (l1 - 1) 
-- d3_1 = d2_1 + (l1 - 1) 
-- d4_0 = d1_0 + (l0 - 1) * 3
-- l1 = l0 + 2
-- dn_1 = [(d1_0 + (l0 - 1) * 3) + ((l0 + 2) - 1) * n | n <- [1..4]]
-- dn_1 = [d1_0 + 3*l0 - 3 + n*l0 + n | n <- [1..4]]
spiralDiags :: [(Integer, [Integer])]
spiralDiags = (1,[1]):iterate diagUpdate (3, [3,5,7,9])
    where
        diagUpdate (l0, (d1_0:_)) = (l0 + 2, [d1_0 + 3*l0 - 3 + n*l0 + n | n <- [1..4]])


spiralDiagsTest = [
    ((+1) $ flip div 4 $ toInteger $ length side, diagonals side) @=? 
    spiralDiags !! n | n <- [0..10], let side = spiral !! n]

spiral :: [[Integer]]
spiral = scanl update [1] [2*4-1,4*4-1..]

spiralTest = [
    [1] @=? (head $ drop 0 spiral),
    [2..9] @=? (head $ drop 1 spiral),
    [10..25] @=? (head $ drop 2 spiral),
    [26..49] @=? (head $ drop 3 spiral)]

update :: [Integer] -> Integer -> [Integer]
update row l = [start..stop]
    where
        start = last row + 1
        stop = start + l

updateTest = [
    [2..9] @=? update [1] (2*4),
    [10..25] @=? update [2..9] (4*4),
    [26..49] @=? update [10..25] (6*4)]

diagonals :: [Integer] -> [Integer]
diagonals [1] = [1]
diagonals row = [row !! (n*l-1) | n <- [1,2,3,4]]
    where
        l = div (length row) 4

diagonalsTest = [
    [ 3,5,7,9 ] @=? diagonals [2..9],
    [ 13,17,21,25 ] @=? diagonals [10..25],
    [ 31,37,43,49 ] @=? diagonals [26..49]]

primes :: [Integer]
primes = 2 : [n | n <- [3,5..], isPrime n]

primesTest = [
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] @=? (take 10 primes),
    [2, 3] @=? (take 2 primes),
    2 @=? (head primes)]

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = all noRemainer primes'
    where
        primes' = takeWhile (\p -> p * p <= n) primes
        noRemainer d = 0 /= rem n d

isPrimeTest = [
    assertBool "9 is not prime" (not $ isPrime 9),
    assertBool "6 is not prime" (not $ isPrime 6),
    assertBool "4 is not prime" (not $ isPrime 4),
    assertBool "0 is not prime" (not $ isPrime 0),
    assertBool "3 is prime" (isPrime 3),
    assertBool "2 is prime" (isPrime 2)]

unitTests = map TestCase $
    spiralTest ++
    diagonalsTest ++
    isPrimeTest ++
    scanPrimesTest ++
    spiralDiagsTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  (_, answer, _) = head $ dropWhile (\(f, _, _) -> f > 0.10) $ problem0058 
    printf "Answer: %d\n" answer
exec AdHoc{..} = do
    let  rows = takeWhile (\(f, _, _) -> f > limit) problem0058
    mapM_ (\(p, _, ns) -> printf "%4.2f: %s\n" p (show ns)) rows
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc {limit = 0.50},
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
