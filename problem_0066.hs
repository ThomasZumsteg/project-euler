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
problem0066 start stop = filter (\(_, d, _) -> (start <= d) && (d <= stop)) solutions

-- x² - Dy² = 1
-- (x² - 1) / y² = D
diophantineSolutions :: Integer -> [(Integer, Integer)]
diophantineSolutions v = map firstAndThird $  filter ((==v) . snd) solutions
    where
        firstAndThird (i, _, k) = (i, k)
        snd (_, j, _) = j

solutions :: [(Integer, Integer, Integer)]
solutions = [(x,d,y) | 
    x <- [2..], y <- [2..x],
    let (d,r) = divMod (x*x-1) (y*y), r == 0]

diophantineSolutionsTest = [
    (3,2) @=? (head $ diophantineSolutions 2),
    (7,4) @=? (head $ diophantineSolutions 3),
    (9,4) @=? (head $ diophantineSolutions 5),
    (5,2) @=? (head $ diophantineSolutions 6 ),
    (8,3) @=? (head $ diophantineSolutions 7 ),
    (17,6) @=? (head $ diophantineSolutions 8 ),
    (19,6) @=? (head $ diophantineSolutions 10),
    (10,3) @=? (head $ diophantineSolutions 11),
    (9801,1820) @=? (head $ diophantineSolutions 29)
    ]

nthRootOrNothing :: Integer -> Integer -> Maybe Integer
nthRootOrNothing = error "Not implemented"

nthRootOrNothingTest = [
    Nothing @=? nthRootOrNothing 2 3,
    Just 3 @=? nthRootOrNothing 3 27,
    Just 4 @=? nthRootOrNothing 2 16
    ]
    
unitTests = map TestCase $
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

