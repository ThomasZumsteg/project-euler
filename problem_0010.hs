{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf
import System.Console.CmdArgs
import Data.Time
import Data.List
import Data.Numbers.Primes

-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

data EulerArgs = 
    Limit { upper :: Integer }
    | Euler 
        deriving (Show, Data, Typeable)
    
limits = Limit{ upper = 10 }
euler = Euler{}

problem0010 :: (Integral a) => a -> a
problem0010 l = sum $ takeWhile ((>) l) primes

exec :: EulerArgs -> Integer 
exec Limit{..} = problem0010 upper
exec Euler = problem0010 2000000

main :: IO ()
main = do
    args <- cmdArgs $ modes [limits, euler]
    start <- getCurrentTime
    print $ exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
