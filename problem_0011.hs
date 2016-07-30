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

problem0011 :: (Integral a) => [[a]] -> a -> [Direction] -> Maybe a
problem0011 [[]] _ _ = error "Empty matrix"
problem0011 _ [] _ = Nothing
problem0011 grid l dirs = maximum $ map (maximum . getSlices grid l) dirs

getSlices :: (Integral a) => [[a]] -> a -> Direction -> a
getSlices _ _ _ = 0

testGridSmall :: [[Integer]]
testGridSmall = [[1,2],[3,4]]

testGridLarge :: [[Integer]]
testGridLarge = [[n..(n+9)] | n <- [0,10..90]]

problem0011Test :: [Test]
problem0011Test = map TestCase 
    [ Just 1 @=? problem0011 [[1]] 1 [Vertical] ,
    Just 1 @=? problem0011 [[1]] 1 [Horizontal] ,
    Just 1 @=? problem0011 [[1]] 1 [DiagonalUp] ,
    Just 1 @=? problem0011 [[1]] 1 [DiagonalDown]
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
    result <- problem0011 grid 4 directions 
    print result
exec Euler = do
    grid <- parseFile "problem_0011.txt"
    let directions = [DiagonalUp, DiagonalDown, Horizontal, Vertical]
    result <- problem0011 grid 4 directions
    print result
exec UnitTests = do 
    runTestTT $ TestList problem0011Test
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [diag, unittest, euler]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
