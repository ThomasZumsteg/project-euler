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
problem0067 = error "Not implmeneted"

triangleWorker :: [[Integer]] -> [[Integer]]
triangleWorker (r:[]) = [r]
triangleWorker (r:rs) = triangleWorker (r':(tail rs))
    where
        r' = rowWorker r (head rs) 

triangleWorkerTest = [
    [[10,13,15],[7,4],[3]] @=? triangleWorker [[8,5,9,3],[2,4,6],[7,4],[3]],
    [[20, 19],[3]] @=? triangleWorker [[10,13,15],[7,4],[3]],
    [[23]] @=? triangleWorker [[10,13,15],[7,4],[3]]
    ]

rowWorker :: [Integer] -> [Integer] -> [Integer]
rowWorker [] _ = []
rowWorker (x:xs) (y:y':ys) = (max (x + y) (x + y')):(rowWorker xs (y':ys))
rowWorker xs ys = error ("Not defined for " ++ show xs ++ " " ++ show ys)

rowWorkerTest = [
    [3] @=? rowWorker [1] [1,2],
    [1,2,3] @=? rowWorker [0, 0, 0] [0, 1, 2, 3]
    ]

readTriangle :: String -> IO [[Integer]]
readTriangle = error "Not Implemented"

unitTests = map TestCase $
    rowWorkerTest ++
    triangleWorkerTest
    
data Arg = Euler | UnitTest |
    AdHoc { file :: String } 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        lines <- readTriangle "problem_0067.txt"
        let  answer = problem0067 lines
        printf "Answer %d\n" answer
    exec AdHoc{..} = do
        lines <- readTriangle file
        let answer = problem0067 lines
        printf "File: %s\nAnswer: %d" file answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { file = "problem_0067.txt" }]
