{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The sum of the squares of the first ten natural numbers is,
-- 12 + 22 + ... + 102 = 385
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)2 = 552 = 3025
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

import Text.Printf
import System.Console.CmdArgs
import Data.Time

data EulerArgs = 
    Limits { 
        upper :: Integer } 
    | Euler 
        deriving (Show, Data, Typeable)
    
limits = Limits{ upper = 10 }
euler = Euler{}

problem0006 :: (Integral a) => a -> a 
problem0006 limit = (triangleNumber limit) ^ 2 - squarePyramidalNum limit 

triangleNumber :: (Integral a) => a -> a
triangleNumber n = n * (n + 1) `quot` 2

squarePyramidalNum :: (Integral a) => a -> a
squarePyramidalNum n = (2 * n ^ 3 + 3 * n ^ 2 + n) `quot` 6

exec :: EulerArgs -> Integer
exec Limits{..} = problem0006 upper
exec Euler = problem0006 100

main :: IO ()
main = do
    args <- cmdArgs $ modes [limits, euler]
    start <- getCurrentTime
    print $ exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start

