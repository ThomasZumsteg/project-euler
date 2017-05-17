{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

import Data.Maybe (mapMaybe, fromJust, isJust, isNothing)
import qualified Data.Map.Lazy as Map

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
-- x² - Dy² = 1
-- (x² - 1) / D = y²
-- (x² - 1) = D q²
-- (x² - 1) is a multiple of D and a square
problem0066 :: Integer -> Integer -> [(Integer, Integer, Integer)]
problem0066 start stop = [(x, d, y) | 
    d <- [start..stop],
    isNothing $ nthRootOrNothing 2 d, 
    let (x, y) = head $ diophantineSolutions d]

diophantineSolutions :: Integer -> [(Integer, Integer)]
diophantineSolutions v = [(x,y) | 
    x <- [2..], 
    (y,d) <- squareDivisors (x * x - 1), 
    d == v]

diophantineSolutionsTest = [
    (3,2) @=? (head $ diophantineSolutions 2),
    (7,4) @=? (head $ diophantineSolutions 3),
    (9,4) @=? (head $ diophantineSolutions 5),
    (5,2) @=? (head $ diophantineSolutions 6),
    (8,3) @=? (head $ diophantineSolutions 7),
    (9801,1820) @=? (head $ diophantineSolutions 29)
    ]

-- Combinations of (y,d)
-- Change to be cached
squareDivisors :: Integer -> [(Integer, Integer)]
squareDivisors n = [(d, q) | 
    d <- takeWhile ((<n) . (^2)) [1..],
    let (q, r) = divMod n (d * d),
    r == 0, n /= q]

squareDivisorsTest = [
    [(2,3)] @=? squareDivisors 12,
    [] @=? squareDivisors 15,
    [(2,9),(3,4)] @=? squareDivisors 36
    ]

nthRootOrNothing :: Integer -> Integer -> Maybe Integer
nthRootOrNothing nth num = num !? [n ^ nth | n <- [0..]]

nthRootOrNothingTest = [
    Nothing @=? nthRootOrNothing 2 3,
    Just 3 @=? nthRootOrNothing 3 27,
    Just 4 @=? nthRootOrNothing 2 16
    ]

(!?) :: (Ord a) => a -> [a] -> Maybe Integer
needle !? haystack = iter 0 haystack
    where
        iter start (h:hs) = case compare needle h of
            LT -> Nothing
            EQ -> Just start
            _ -> iter (1 + start) hs

searchTest = [
    Just 5 @=? 5 !? [0..10],
    Nothing @=? 5 !? [0,2..10],
    Nothing @=? 5 !? [0,2..],
    Just 5 @=? 5 !? [0,1..]
    ]
    
unitTests = map TestCase $
    squareDivisorsTest ++
    searchTest ++
    nthRootOrNothingTest ++
    diophantineSolutionsTest

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

