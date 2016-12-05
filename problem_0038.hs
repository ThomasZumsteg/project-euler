{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Take the number 192 and multiply it by each of 1, 2, and 3:
--     192 × 1 = 192
--     192 × 2 = 384
--     192 × 3 = 576
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (sort)

data EulerArgs = 
    AdHoc { limit::Int }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0038 :: Integer -> [Integer]
problem0038 = error "Not Implemented"

sortedPermutations :: [a] -> [[a]]
sortedPermutations [] = []
sortedPermutations (c:[]) = [[c]]
sortedPermutations cs = concatMap joinPerms seperated
    where
        seperated = [splitAt n cs | n <- [0..(length cs - 1)]]
        joinPerms (as,(b:bs)) = map ((:) b) $ sortedPermutations (as ++ bs)

sortedPermutationsTest = [
    "987654321" @=? (last $ sortedPermutations "123456789"),
    ["123", "132", "213", "231", "312", "321"] @=? sortedPermutations "123",
    ["12", "21"] @=? sortedPermutations "12",
    ["1"] @=? sortedPermutations "1",
    [] @=? sortedPermutations ""]

unitTests = map TestCase $
    sortedPermutationsTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0038 10
    printf "List of %d pandigital multiples is:\n" limit
    mapM_ (printf "%d\n")  answer
exec Euler = do
    let answer = head $ problem0038 1
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ limit = 100 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
