{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (nub)
import qualified Data.Map as M

data EulerArgs =
    Euler
    | AdHoc{ len::Int }
    | UnitTest
    | RollUp{ start::Int, size::Int }
    deriving (Show, Data, Typeable)

-- By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
-- By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.
-- Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

problem0051 :: Int -> [Integer]
problem0051 l = head $ filter ((==l) . length) $ rollUp M.empty primePatterns 
    where
        primePatterns = [(pat, p) | p <- primes, pat <- (patters '.' $ show p)]

rollUp :: (Ord a) => M.Map a [b] -> [(a, b)] -> [[b]]
rollUp _ [] = []
rollUp cache ((k, v):vks) = vs : (rollUp cache' vks)
    where
        vs = v : M.findWithDefault [] k cache
        cache' = M.insertWith (++) k [v] cache

rollUpTest = [
    [] @=? rollUp M.empty ([]::[(Int,Int)]),
    [[1],[2],[3,1],[4,2]] @=? rollUp M.empty [(1,1),(0,2),(1,3),(0,4)]]

substitue :: (Eq a) => a -> a -> [a] -> [[a]]
substitue _ _ [] = [[]]
substitue char r (t:text)
    | t /= char = map ((:)t) remainer
    | t == char = (map ((:)t) remainer) ++ (map ((:)r) remainer)
    where
        remainer = substitue char r text

substitueTest = [
    [""] @=? substitue 'a' '.' "",
    ["11", "1.", ".1", ".."] @=? substitue '1' '.' "11",
    ["aba","ab.",".ba",".b."] @=? substitue 'a' '.' "aba",
    ["a","."] @=? substitue 'a' '.' "a"]

patters :: (Eq a) => a -> [a] -> [[a]]
patters marker text = tail $ nub $ concat [substitue i marker text | i <- nub text]

pattersTest = [
    ["."] @=? patters '.' "1",
    ["1.",".1",".."] @=? patters '.' "11",
    ["12.",".21",".2.","1.1"] @=? patters '.' "121",
    ["123.",".231",".23.","1.31","12.1"] @=? patters '.' "1231"]

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
    isPrimeTest ++
    rollUpTest ++
    substitueTest ++
    pattersTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = problem0051 8
    printf "Answer: %d\n" (last answer)
exec AdHoc{..} = do
    let answer = problem0051 len
    printf "Answer: %d\n" (last answer)
    print answer
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()
exec RollUp{..}= do
    let sets = take size $ drop start [(pat, p) | p <- primes, pat <- (patters '.' $ show p)]
    mapM_ print sets

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc{ len=7 },
        UnitTest,
        RollUp{ start = 0, size = 10 }]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
