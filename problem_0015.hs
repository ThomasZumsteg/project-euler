{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf (printf)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (IOMode( ReadMode), hGetContents, openFile)
import Data.Char (digitToInt)

-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

-- How many such routes are there through a 20×20 grid?

problem0015 :: (Integral a) => (a, a) -> Integer
problem0015 (0, 0) = 0
problem0015 (_, 0) = 1
problem0015 (0, _) = 1
problem0015 (x, y) = problem0015 (x-1, y) + problem0015 (x, y-1)

problem0015Test :: [Test]
problem0015Test = map TestCase [
    0 @=? problem0015 ( 0,  0),
    1 @=? problem0015 (17,  0),
    1 @=? problem0015 ( 0, 16),
    6 @=? problem0015 ( 2,  2)
    ]

data EulerArgs = 
    AdHoc { gridSize :: (Int, Int) }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = problem0015 gridSize
        x = fst gridSize
        y = snd gridSize
    printf "In a %d x %d grid there are %d paths\n" x y answer
    printf "from the upper left to the lower right\n"
exec Euler = do
    let answer = problem0015 (20, 20)
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList $ problem0015Test
    return ()

adHoc = AdHoc{ gridSize = (2,2) }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
