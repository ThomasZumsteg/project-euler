{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf
import System.Console.CmdArgs
import Data.Time
import System.IO
import Data.Char
import Test.HUnit

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

problem0011 :: String -> [Direction] -> Integer
problem0011 path ds = 0

getSlices :: (Integral a) => [[a]] -> Direction -> [[a]]
getSlices grid Horizontal = grid
getSlices [] Vertical = []
getSlices grid Vertical = map head grid : getSlices ( map tail grid ) Vertical
getSlices grid DiagonalDown = [[]]
getSlices grid DiagonalUp = [[]]

tests :: [Test]
tests = [ TestCase (assertEqual "Empty grid" [[]] (getSlices [[]] Horizontal))
    ,TestCase (assertEqual "Easy grid" [[1,2],[3,4]] (getSlices [[1,2],[3,4]] Horizontal))
    ,TestCase (assertEqual "Easy Vertical" [[1,3],[2,4]] (getSlices [[1,2],[3,4]] Vertical))
    ,TestCase (assertEqual "Easy DiagonalUp" [[1],[2,3],[4]] (getSlices [[1,2],[3,4]] DiagonalUp))
    ,TestCase (assertEqual "Easy DiagonalDown" [[2],[1,3],[4]] (getSlices [[1,2],[3,4]] DiagonalDown))]

parseFile :: String -> IO [[Integer]]
parseFile name = do
    file <- openFile name ReadMode
    contents <- hGetContents file
    return $ map (map read . words) $ lines contents

exec :: EulerArgs -> IO ()
exec Diagonal{..} = print $ problem0011 path [DiagonalUp, DiagonalDown]
exec Euler = print $ problem0011 "problem_0011.txt" [DiagonalUp, DiagonalDown, Horizontal, Vertical]
exec UnitTests = do
    runTestTT $ TestList tests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [diag, unittest, euler]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
