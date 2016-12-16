{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
-- Let d_1 be the 1st digit, d_2 be the 2nd digit, and so on. In this way, we note the following:
--     d_2 d_3 d_4 = 406 is divisible by 2
--     d_3 d_4 d_5 = 063 is divisible by 3
--     d_4 d_5 d_6 = 635 is divisible by 5
--     d_5 d_6 d_7 = 357 is divisible by 7
--     d_6 d_7 d_8 = 572 is divisible by 11
--     d_7 d_8 d_9 = 728 is divisible by 13
--     d_8 d_9 d_10 = 289 is divisible by 17
-- Find the sum of all 0 to 9 pandigital numbers with this property.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs = 
    AdHoc 
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0043 :: [Integer]
problem0043 = map read $ filter hasProperty $ sortedPermutations "0123456789"

hasProperty :: String -> Bool
hasProperty digits = all property $ zip [1..] primes
    where
        primes = [2,3,5,7,11,13,17]
        property (n, p) = div_n_by_m p $ read $ take 3 $ drop n digits
        div_n_by_m m n = 0 == mod n m

hasPropertyTest = [
    False @=? hasProperty "1046357289",
    True  @=? hasProperty "1406357289"]

sortedPermutations :: [a] -> [[a]]
sortedPermutations [] = []
sortedPermutations (c:[]) = [[c]]
sortedPermutations cs = concatMap joinPerms seperated
    where
        seperated = [splitAt n cs | n <- [0..(length cs - 1)]]
        joinPerms (as,(b:bs)) = map ((:) b) $ sortedPermutations (as ++ bs)

sortedPermutationsTest = [
    "123456789" @=? (last $ sortedPermutations "987654321"),
    "987654321" @=? (last $ sortedPermutations "123456789"),
    ["123", "132", "213", "231", "312", "321"] @=? sortedPermutations "123",
    ["12", "21"] @=? sortedPermutations "12",
    ["1"] @=? sortedPermutations "1",
    [] @=? sortedPermutations ""]

unitTests = map TestCase $
    sortedPermutationsTest ++
    hasPropertyTest

exec :: EulerArgs -> IO ()
exec AdHoc = do
    let answer = problem0043 
    printf "Pandigital numbers with property:\n" 
    mapM_ (printf "%d\n") answer
exec Euler = do
    let answer = sum problem0043
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
