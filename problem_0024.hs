{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

-- 012   021   102   120   201   210

-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs = 
    AdHoc { element::Integer, items::String}
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0024 :: [a] -> Integer -> a
problem0024 = error "Not implemented"

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations items = concat $ map ps $ makeFirst [] items
    where
        ps (i:is) = map (i:) (permutations is)

permutationsTest = [
    ["0123", "0132", "0213", "0231", "0312", "0321",
     "1023", "1032", "1203", "1230", "1302", "1320",
     "2013", "2031", "2103", "2130", "2301", "2310",
     "3012", "3021", "3102", "3120", "3201", "3210"
    ] @=? permutations "0123",
    ["012", "021", "102", "120", "201", "210" ] @=? permutations "012",
    ["01", "10"] @=? permutations "01",
    ["0"] @=? permutations "0",
    [[]] @=? permutations ([]::[Char]) ]

makeFirst :: [a] -> [a] -> [[a]]
makeFirst _ [] = []
makeFirst pre (i:post) = ((i : pre) ++ post) : (makeFirst (pre ++ [i]) post)

makeFirstTest = [
     ["abc", "bac", "cab"] @=? makeFirst "" "abc",
     [] @=? makeFirst "" ""]

insertAt :: a -> [a] -> Int -> [a]
insertAt item items n 
    | n < 0 || length items < n = error $ "Must be in range 0 to " ++ (show $ length items)
    | otherwise = fore ++ [item] ++ aft
    where
        fore = take n items
        aft = drop n items

insertAtTest = [
    "abcd" @=? insertAt 'd' "abc" 3,
    "abcd" @=? insertAt 'c' "abd" 2,
    "abcd" @=? insertAt 'b' "acd" 1,
    "abcd" @=? insertAt 'a' "bcd" 0,
    "a" @=? insertAt 'a' "" 0 ]

unitTests = map TestCase $
    insertAtTest ++
    makeFirstTest ++ 
    permutationsTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0024 items element
    printf  "The %d permutation of %s is %s" element items answer
exec Euler = do
    let answer = problem0024 "0123456789" 1000000
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ element = 1000000, items = "0123456789"}, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
