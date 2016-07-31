{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf
import System.Console.CmdArgs
import Data.Time
import System.IO
import Data.Char
import Test.HUnit ((@=?), runTestTT, Test(..))
import Data.List

-- In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
-- The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
-- What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?

data Direction = Horizontal | Vertical | DiagonalUp | DiagonalDown

data EulerArgs = 
    Diagonal { path :: String }
    | UnitTests 
    | Euler 
        deriving (Show, Data, Typeable)
    
diag = Diagonal { path = "problem_0011.txt" }
unittest = UnitTests{}
euler = Euler{}

problem0011 :: (Integral a) => [[a]] -> Int -> [Direction] -> Maybe a
problem0011 [[]] _ _ = error "Empty matrix"
problem0011 _ _ [] = Nothing
problem0011 grid l dirs = Just $ maximum $ map product slices
    where
        rows = length grid
        cols = length $ head grid
        points = [(r,c) | r <- [0..(rows - 1 - l)], c <- [0..(cols - 1 - l)]]
        slices = map (map (getSlice grid l) dirs) points

testGridSmall :: [[Integer]]
testGridSmall = [[1,2],[3,4]]

testGridLarge :: [[Integer]]
testGridLarge = [[n..(n+9)] | n <- [0,10..90]]

problem0011Test :: [Test]
problem0011Test = map TestCase [ 
    Just 1 @=? problem0011 [[1]] 1 [Vertical] ,
    Just 1 @=? problem0011 [[1]] 1 [Horizontal] ,
    Just 1 @=? problem0011 [[1]] 1 [DiagonalUp] ,
    Just 1 @=? problem0011 [[1]] 1 [DiagonalDown],
    Just 8 @=? problem0011 testGridSmall 2 [Vertical] ,
    Just 12 @=? problem0011 testGridSmall 2 [Horizontal] ,
    Just 6 @=? problem0011 testGridSmall 2 [DiagonalUp] ,
    Just 4 @=? problem0011 testGridSmall 2 [DiagonalDown],
    Just 12 @=? problem0011 testGridSmall 2 
        [Vertical, Horizontal, DiagonalUp, DiagonalDown]
    ] 

type Point = (Int, Int)

getSlice :: (Integral a) => [[a]] -> a -> Direction -> Point -> [a]
getSlice grid l Vertical (r, c) = [grid !! r + n !! c | n <- [0..l]]
getSlice grid l Horizontal (r, c) = [grid !! r !! c + n | n <- [0..l]]
getSlice grid l DiagonalDown (r, c) = [grid !! r + n !! c + n | n <- [0..l]]
getSlice grid l DiagonalUp (r, c) = [grid !! r + n !! c - n | n <- [0..l]]

getSliceTest :: [Test]
getSliceTest = map TestCase [
    [1,3] @=? getSlice testGridSmall 2 Vertical (0,0), 
    [1,2] @=? getSlice testGridSmall 2 Horizontal (0,0), 
    [2,3] @=? getSlice testGridSmall 2 DiagonalUp (0,1),
    [1,4] @=? getSlice testGridSmall 2 DiagonalDown (0,0)
    ]

parseFile :: String -> IO [[Integer]]
parseFile name = do
    file <- openFile name ReadMode
    contents <- hGetContents file
    return $ map (map read . words) $ lines contents

exec :: EulerArgs -> IO ()
exec Diagonal{..} = do
    grid <- parseFile path
    let directions = [DiagonalUp, DiagonalDown]
        result = problem0011 grid 4 directions 
    print result
exec Euler = do
    grid <- parseFile "problem_0011.txt"
    let directions = [DiagonalUp, DiagonalDown, Horizontal, Vertical]
        result = problem0011 grid 4 directions
    print result
exec UnitTests = do 
    runTestTT $ TestList $ problem0011Test ++ sliceTest
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [diag, unittest, euler]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
