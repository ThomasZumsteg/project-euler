{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

import Data.Maybe (mapMaybe, fromJust, isJust, isNothing)

-- Consider quadratic Diophantine equations of the form:
-- x² – Dy² = 1
-- For example, when D=13, the minimal solution in x is 649² – 13×180² = 1.
-- It can be assumed that there are no solutions in positive integers when D is square.
-- By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
-- 3² – 2×2² = 1
-- 2² – 3×1² = 1
-- 9² – 5×4² = 1
-- 5² – 6×2² = 1
-- 8² – 7×3² = 1
-- Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
-- Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.

-- (x, d, y)
problem0066 :: Integer -> Integer -> [(Integer, Integer, Int)]
problem0066 start stop = [ head $ [(x, d, fromJust y) |
        x <- [2..], 
        let (q, r) = divMod (x*x - 1) d,
        let y = logSearch q squares,
        r == 0,
        isJust y ] | 
    d <- [start..stop],
    isNothing $ logSearch d squares]
    where
        squares = [i*i | i <- [0..]]

logSearch :: (Ord a) => a -> [a] -> Maybe Int
logSearch needle haystack = worker 0 0
    where
        worker i j
            | j < 0 = Nothing
            | haystack !! i == needle = Just i
            | haystack !! i < needle && haystack !! (i+j^2) < needle =
                worker (i+j+1) (j+1)
            | otherwise =
                worker i (j-1)

logSearchTest = [
    Just 6 @=? logSearch 7 [1..],
    Nothing @=? logSearch 7 [2,4..]
    ]
        

unitTests = map TestCase $
    logSearchTest

data Arg = Euler | UnitTest |
    AdHoc {start :: Integer, stop :: Integer} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let (answer, _, _) = minimum $ problem0066 0 1000
        printf "Answer %d\n" answer
    exec AdHoc{..} = do
        let answer = problem0066 start stop
        mapM_ (\(x,d,y) -> printf "%d² - %dx%d²=1\n" x d y) answer 
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {start=2, stop=10}]

