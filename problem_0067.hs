{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

-- By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3

-- That is, 3 + 7 + 4 + 9 = 23.

-- Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.

-- NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, as there are 299 altogether! If you could check one trillion (1012) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)

problem0067 :: [[Integer]] -> Integer
problem0067 = head . foldl1 rowWorker

rowWorker :: [Integer] -> [Integer] -> [Integer]
rowWorker [] _ = []
rowWorker (x:x':xs) (y:ys) = (max (x + y) (x' + y)):(rowWorker (x':xs) ys)
rowWorker xs ys = error ("Not defined for rowWorker" ++ show xs ++ " " ++ show ys)

rowWorkerTest = [
    [3] @=? rowWorker [1,2] [1],
    [1,2,3] @=? rowWorker [0, 1, 2, 3] [0, 0, 0]
    ]

readTriangle :: String -> IO [[Integer]]
readTriangle fn = reverse <$> map ((map read) . words) <$> lines <$> readFile fn

unitTests = map TestCase $
    rowWorkerTest
    
data Arg = Euler | UnitTest |
    AdHoc { file :: String } 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        lines <- readTriangle "problem_0067.txt"
        let answer = problem0067 lines
        printf "Answer %d\n" answer
    exec AdHoc{..} = do
        let lines = [[8,5,9,3],[2,4,6],[7,4],[3]]
            answer = problem0067 lines
        printf "Answer: %d\n" answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { file = "problem_0067.txt" }]
