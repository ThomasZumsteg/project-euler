{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

-- A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs = 
    AdHoc { limit::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0023 :: Integer -> Integer
problem0023 limit = error "Not implemented"

byFirstElement :: (Ord a) => [a] -> [a] -> Bool
byFirstElement (n:_) [] = True
byFirstElement [] (m:_) = False
byFirstElement (n:_) (m:_) = n > m


mergeAndSortLists :: [[Integer]] -> [Integer]
mergeAndSortLists [] = []
mergeAndSortLists ([]:ls) = mergeAndSortLists ls
mergeAndSortLists ((i:is):ls) = i : remainer 
    where
        remainer = mergeAndSortLists $ sortedInsert byFirstElement is ls

mergeAndSortListsTest = [
    [1,2,2,3,3,4,4,4,5,5] @=? (take 10 $ 
        mergeAndSortLists [[n,n*2..] | n <- [1..10]]),
    [1,2,2,3,3,4,4,4,5,5] @=? (take 10 $ 
        mergeAndSortLists [[n,n*2..n*10] | n <- [1..10]]),
    [1,2,3] @=? mergeAndSortLists [[n] | n <- [1,2,3]],
    [1] @=? mergeAndSortLists [[1]],
    [] @=? mergeAndSortLists [[]],
    [] @=? mergeAndSortLists []
    ]

sortedInsert :: (a -> a -> Bool) -> a -> [a] -> [a]
sortedInsert _ i [] = [i]
sortedInsert f i ls@(l:ls') 
    | f i l = l : (sortedInsert f i ls')
    | otherwise = i : ls

sortedInsertTest = [
    [[1,2,3],[2,4,6],[3,6,9]] @=? map (take 3) (take 3 $ 
        sortedInsert byFirstElement [2,4..] [[n,(2*n)..] | n <- [1,3..]]),
    [[1,2,3],[2,4,6],[3,6,9]] @=? map (take 3) (take 3 $ 
        sortedInsert byFirstElement [2,4..] [[1,2..],[3,6..]]),
    [[1],[2],[3]] @=? sortedInsert byFirstElement [2] [[1],[3]],
    [1,2,3] @=? (take 3 $ sortedInsert (>) 2 [1,3..]),
    [1,2,3] @=? sortedInsert (>) 1 [2,3],
    [1,2,3] @=? sortedInsert (>) 2 [1,3],
    [1,2,3] @=? sortedInsert (>) 3 [1,2],
    [1] @=? sortedInsert (<) 1 []
    ]

abundantNumberPairsSum :: [[Integer]]
abundantNumberPairsSum = [sublist $ drop n abundantNumbers | n <- [0..]]
    where
        sublist as@(a:_) = map (a+) as

abundantNumberPairsSumTest = [
    [36,38,42] @=? (take 3 $ abundantNumberPairsSum !! 1),
    [24,30,32] @=? (take 3 $ abundantNumberPairsSum !! 0),
    [24] @=? (take 1 $ head abundantNumberPairsSum),
    24 @=? (head $ head abundantNumberPairsSum)
    ]

abundantNumbers :: [Integer]
abundantNumbers = filter isAbundant [1..]
    where
        isAbundant n = n < (sum $ properFactors n)

abundantNumbersTest = [
    420 @=? abundantNumbers !! 100,
    20 @=? abundantNumbers !! 2,
    18 @=? abundantNumbers !! 1,
    12 @=? head abundantNumbers
    ]

properFactors :: Integer -> [Integer]
properFactors n = filter noRem [1..(n-1)]
    where
        noRem = (==) 0 . rem n

properFactorsTest = [
    [] @=? properFactors 1,
    [1] @=? properFactors 13,
    [1,2,3,4,6] @=? properFactors 12
    ]

unitTests = map TestCase $
    properFactorsTest ++
    abundantNumbersTest ++
    abundantNumberPairsSumTest ++
    sortedInsertTest ++
    mergeAndSortListsTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0023 limit
    printf  "The sum of all non-abundant numbers below %d is %d\n" limit answer
exec Euler = do
    let answer = problem0023 28123 
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ limit = 28123 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
