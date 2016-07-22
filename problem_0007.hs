{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf
import System.Console.CmdArgs
import Data.Time

-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

data EulerArgs = 
    Limits { 
        upper :: Integer } 
    | Euler 
        deriving (Show, Data, Typeable)
    
limits = Limits{ upper = 6 }
euler = Euler{}

problem0007 :: (Integral a) => a -> a 
problem0007 = (!!) primes . fromIntegral

primes :: (Integral a) => [a]
primes = 2 : filter isPrime [3, 5..]
  where
    isPrime p = all ((/=) 0 . mod p) $ takeWhile (\n -> n * n <= p) primes

exec :: EulerArgs -> Integer
exec Limits{..} = problem0007 upper
exec Euler = problem0007 10001

main :: IO ()
main = do
    args <- cmdArgs $ modes [limits, euler]
    start <- getCurrentTime
    print $ exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start

