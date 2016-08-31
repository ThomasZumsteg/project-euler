{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3

-- That is, 3 + 7 + 4 + 9 = 23.

-- Find the maximum total from top to bottom of the triangle below:

-- 75
-- 95 64
-- 17 47 82
-- 18 35 87 10
-- 20 04 82 47 65
-- 19 01 23 75 03 34
-- 88 02 77 73 07 63 67
-- 99 65 04 28 06 16 70 92
-- 41 41 26 56 83 40 80 70 33
-- 41 48 72 33 47 32 37 16 94 29
-- 53 71 44 65 25 43 91 52 97 51 14
-- 70 11 33 28 77 73 17 78 39 68 17 57
-- 91 71 52 38 17 14 91 43 58 50 27 29 48
-- 63 66 04 68 89 53 67 30 73 16 69 87 40 31
-- 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

-- NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)

import Text.Printf (printf, PrintfArg)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (IOMode( ReadMode), hGetContents, openFile)
import Data.Char (digitToInt, isAlpha)
import Data.Maybe (fromJust)

problem0018 :: [[Integer]] -> Integer
problem0018 = fastSolve

maxPath :: Maybe TreeNode -> Integer
maxPath Nothing = 0
maxPath (Just TreeNode {left=left, right=right, node=node}) =
    node + (max (maxPath left) (maxPath right))

readRows :: String -> IO [[Integer]]
readRows filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let triangle = map (map read . words) $ lines contents
    return triangle

data TreeNode = TreeNode { 
    left :: Maybe TreeNode, 
    right :: Maybe TreeNode,
    node :: Integer }
    deriving (Eq, Show)

buildTriangleTree :: [[Integer]] -> Maybe TreeNode
buildTriangleTree [] = Nothing
buildTriangleTree ([]:_) = Nothing
buildTriangleTree (row:rows) = Just $ TreeNode left right value
    where
        value = head row
        left = buildTriangleTree rows
        right = buildTriangleTree $ map tail rows

fastSolve :: [[Integer]] -> Integer
fastSolve [[]] = 0
fastSolve rows = maximum $ foldr1 maxRowPath rows

maxRowPath :: [Integer] -> [Integer] -> [Integer]
maxRowPath rowM@(m:ms) rowN@(nl:nr:ns)
    | lenM + 1 == lenN && lenM /= 0 = node:rest
    where
        lenN = length rowN
        lenM = length rowM
        node = max (m + nl) (m + nr) 
        rest = maxRowPath ms (nr:ns)
maxRowPath _ _ = []

testTriangle :: [[Integer]]
testTriangle = [ [ 3 ],
    [ 7, 4],
    [ 2, 4, 6],
    [ 8, 5, 9, 3] ]

problem0018Test :: [Test]
problem0018Test = map TestCase [
    23 @=? problem0018 testTriangle
    ]

buildTriangleTreeTest :: [Test]
buildTriangleTreeTest = map TestCase [
    Just ( TreeNode 
        (Just $ TreeNode 
            (Just $ TreeNode Nothing Nothing 4)
            (Just $ TreeNode Nothing Nothing 5)
            2)
        (Just $ TreeNode 
            (Just $ TreeNode Nothing Nothing 5)
            (Just $ TreeNode Nothing Nothing 6)
            3)
        1) @=? buildTriangleTree [[1],[2,3],[4,5,6]],
    Just ( TreeNode 
        (Just $ TreeNode Nothing Nothing 2)
        (Just $ TreeNode Nothing Nothing 3)
        1) @=? buildTriangleTree [[1],[2,3]],
    Just (TreeNode Nothing Nothing 1) @=? buildTriangleTree [[1]],
    Nothing @=? buildTriangleTree []
    ]

maxPathTest :: [Test]
maxPathTest = map TestCase [
    23 @=? (maxPath $ buildTriangleTree testTriangle),
    4 @=? (maxPath $ buildTriangleTree [[1],[2,3]]),
    1 @=? (maxPath $ buildTriangleTree [[1]]),
    0 @=? (maxPath $ buildTriangleTree [[]])
    ]

fastSolveTest :: [Test]
fastSolveTest = map TestCase [
    23 @=? fastSolve testTriangle,
    10 @=? fastSolve [[1],[2,3],[4,5,6]],
    4 @=? fastSolve [[1],[2,3]],
    1 @=? fastSolve [[1]],
    0 @=? fastSolve [[]]
    ]

maxRowPathTest :: [Test]
maxRowPathTest = map TestCase [
    [6,8,10] @=? maxRowPath [1,2,3] [4,5,6,7],
    [7,9] @=? maxRowPath [2,3] [4,5,6],
    [4] @=? maxRowPath [1] [2,3],
    [] @=? maxRowPath [] [1]
    ]

data EulerArgs = 
    AdHoc { file :: String }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    rows <- readRows file
    let answer = problem0018 rows
    printf "The maximum path is %d\n" answer
exec Euler = do
    rows <- readRows "problem_0018.txt"
    let answer = problem0018 rows
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList $ problem0018Test ++ buildTriangleTreeTest ++ maxPathTest ++ fastSolveTest ++ maxRowPathTest
    return ()

adHoc = AdHoc{ file="problem_0018.txt" }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
