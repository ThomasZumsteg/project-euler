{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf
import System.Console.CmdArgs
import Data.Time
import System.IO
import Data.Char

-- The four adjacent digits in the 1000-digit number that have the greatest product are 9 × 9 × 8 × 9 = 5832.
-- Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?


-- data EulerArgs = 
--     Limits { 
--         digits :: Integer,
--         path :: String }
--     | Euler 
--         deriving (Show, Data, Typeable)
    
-- limits = Limits{ digits = 6,
--     path = "problem_0008.txt" }
-- euler = Euler{}

problem0008 :: (Integral a) => Int -> [a] -> a 
problem0008 n d = maximum $ map product $ createGroups n d

-- exec :: EulerArgs -> Integer
-- exec Limits{..} = do
--     contents <- readFile path
--     print contents
--     return 0
-- exec Euler = problem0008 0 0 

-- main :: IO ()
-- main = do
--     args <- cmdArgs $ modes [limits, euler]
--     start <- getCurrentTime
--     print $ exec args
--     stop <- getCurrentTime
--     print $ diffUTCTime stop start

createGroups :: Int -> [a] -> [[a]]
createGroups n items@(i:is)
    | n > length items = []
    | otherwise = firstN : createGroups n is
    where
        firstN = take n items

main :: IO ()
main = do
    handle <- openFile "problem_0008.txt" ReadMode
    contents <- hGetContents handle 
    let digits = map digitToInt $ filter ((/=) '\n') contents
    print $ maximum $ map product $ createGroups 13 digits
