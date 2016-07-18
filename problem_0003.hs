{-# LANGUAGE DeriveDataTypeable #-}

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

import Text.Printf
import System.Console.CmdArgs
import Data.Time

data EulerArgs = EulerArgs { limit :: Integer }
    deriving (Show, Data, Typeable)
    
euler = EulerArgs { limit = 600851475143 }

problem0003 :: (Integral a) => a -> a 
problem0003 = last . findFactors primes

findFactors :: (Integral a) => [a] -> a -> [a]
findFactors _ 1 = []
findFactors [] n = error "Not enough factors"
findFactors primes@(p:ps) num
    | 0 == mod num p = p : findFactors primes (quot num p)
    | otherwise      = findFactors ps num

primes :: (Integral a) => [a]
primes = 2 : filter isPrime [3, 5..]
  where
    isPrime p = not $ any ((==) 0 . mod p) $ takeWhile (\n -> n * n <= p) primes

main :: IO ()
main = do
    args <- cmdArgs euler
    let EulerArgs{ limit = l } = args
    start <- getCurrentTime
    print $ problem0003 l
    stop <- getCurrentTime
    print $ diffUTCTime stop start

