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

problem0011 :: String -> [Direction] -> Integer
problem0011 path ds = 0

data Coord = Coord { row :: Int, col :: Int }

getSlices :: (Integral a) => Direction -> [[a]] -> [[a]]
getSlices Horizontal = id
getSlices Vertical = transpose
getSlices DiagonalDown = map (getCoords diagonalDown) 
    where
        getCoords Coord{row=r, col=c} matrix = matrix !! r !! c
getSlices DiagonalUp = map (getCoords diagonalUp)
    where
        getCoords Coord{row=r, col=c} matrix = matrix !! r !! c

diagonalDown :: [[Coord]]
diagonalDown = map ( map (\(r, c) -> Coord{row=r,col=c}) ) [ [(0,1)],
    [(0,0), (1,1)],
    [(1,0)] ]

diagonalUp :: [[Coord]]
diagonalUp = map (map (\(r, c) -> Coord{row=r,col=c})) [ [(0,0)],
    [(1,0), (0,1)],
    [(1,1)] ]

getSlicesTests :: [Test]
getSlicesTests = map TestCase 
    [ [[]] @=? getSlices Horizontal [[]],
    [[1,2], [3,4]] @=? getSlices Horizontal [[1,2],[3,4]],
    [[1,3], [2,4]] @=? getSlices Vertical [[1,2],[3,4]],
    [[1], [3,2], [4]] @=? getSlices DiagonalUp [[1,2],[3,4]],
    [[3], [1,4], [2]] @=? getSlices DiagonalDown [[1,2],[3,4]]
    ]

parseFile :: String -> IO [[Integer]]
parseFile name = do
    file <- openFile name ReadMode
    contents <- hGetContents file
    return $ map (map read . words) $ lines contents

exec :: EulerArgs -> IO ()
exec Diagonal{..} = print $ problem0011 path [DiagonalUp, DiagonalDown]
exec Euler = print $ problem0011 "problem_0011.txt" [DiagonalUp, DiagonalDown, Horizontal, Vertical]
exec UnitTests = do
    runTestTT $ TestList getSlicesTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [diag, unittest, euler]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
