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
problem0011 path = map (getSlices grid) 

getSlices :: (Integral a) => [[a]] -> Direction -> [[a]]
getSlices grid Horizontal = grid
getSlices grid Vertical = [[]]
getSlices grid DiagonalDown = [[]]
getSlices grid DiagonalUp = [[]]

parseFile :: String -> IO [[Integer]]
parseFile name = do
    file <- openFile name ReadMode
    contents <- hGetContents file
    return $ map (map read . words) $ lines contents

exec :: EulerArgs -> IO ()
exec Diagonal{..} = print $ problem0011 DiagonalOnly path
exec Euler = print $ problem0011 All "problem_0011.txt"
exec UnitTests = do
    runTestTT $ TestList tests
    return ()

tests :: [Test]
tests = [ TestCase $ ]

main :: IO ()
main = do
    args <- cmdArgs $ modes [diag, unittest, euler]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
