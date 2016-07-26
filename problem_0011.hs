{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf
import System.Console.CmdArgs
import Data.Time
import System.IO
import Data.Char

-- In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
-- The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
-- What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?

data Direction = All | DiagonalOnly

data EulerArgs = 
    Diagonal { path :: String }
    | Euler 
        deriving (Show, Data, Typeable)
    
diag = Diagonal { path = "problem_0011.txt" }
euler = Euler{}

problem0011 :: Direction -> String -> Integer
problem0011 All path = maximum $ (diagonal ++ horizontal ++ vertical)
    where
        grid = do parseFile path
        diagonal = [1..10]
        horizontal = [1..10]
        vertical= [1..10]
problem0011 DiagonalOnly path = maximum $ diagonal
    where
        grid = do parseFile path
        diagonal = [1..10]

parseFile :: String -> IO [[Integer]]
parseFile name = do
    file <- openFile name ReadMode
    contents <- hGetContents file
    return $ map (map read . words) $ lines contents

exec :: EulerArgs -> Integer 
exec Diagonal{..} = problem0011 DiagonalOnly path
exec Euler = problem0011 All "problem_0011.txt"

main :: IO ()
main = do
    args <- cmdArgs $ modes [diag, euler]
    start <- getCurrentTime
    print $ exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
