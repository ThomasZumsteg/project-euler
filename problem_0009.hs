{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf
import System.Console.CmdArgs
import Data.Time

-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

data EulerArgs = 
    Sum { 
        tripletSum :: Integer
        }
    | Euler 
        deriving (Show, Data, Typeable)
    
limits = Sum{ tripletSum = 12 }
euler = Euler{}

-- a + b + c == n
-- c = n - a - b
-- a^2 + b^2 == c^2
-- (n - a - b)^2 + b^2 == c^2
-- (n - a - b)^2 + b^2 == c^2
problem0009 :: (Integral a) => a -> a
problem0009 s = tripProd triplet
    where
        tripProd (x,y,z) = x * y * z
        triplet = head $ filter (\(a,b,c) -> a + b + c == s) triplets
        triplets = [(c,b,s - c - b) | c <- [1..], b <- [1..c], checkTriplet c b]
        checkTriplet c' b' = (s - c' - b')^2 + b'^2 == c'^2

isTriplet :: (Integral a) => (a,a,a) -> Bool
isTriplet (a, b, c) = a^2 + b^2 == c^2

exec :: EulerArgs -> Integer 
exec Sum{..} = problem0009 tripletSum
exec Euler = problem0009 1000

main :: IO ()
main = do
    args <- cmdArgs $ modes [limits, euler]
    start <- getCurrentTime
    print $ exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
