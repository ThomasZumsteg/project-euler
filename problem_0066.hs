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
    not $ elem d squares,
    let (x, y) = (0,0)]
    where
        squares = [n*n | n <- [1..stop]]

sumFractionExpantion :: (Integer, [Integer]) -> (Integer, Integer)
sumFractionExpantion (x, xs) = (\(n, d) -> (x * d + n, n)) (foldr worker (0,0) xs)
    where
        worker n (m, e) = (e * n + m, m)

compressedSqrtExpansion :: Integer -> (Integer,[Integer])
compressedSqrtExpansion = fsts . compress . sqrtFraction
    where
        fsts (x, xs) = (fst x, map fst xs)
        fst (x, _, _) = x

compress :: (Eq a) => [a] -> (a,[a])
compress (x:x':xs) = (x, x' : takeWhile (/=x') xs)

compressTest = [
    (1, [1]) @=? (compress $ repeat 1),
    (2, [3, 2]) @=? (compress $ cycle [2, 3])
    ]

sqrtFraction :: Integer -> [(Integer, Integer, Integer)]
sqrtFraction sq = iterate worker (0, 0, 1)
    where
        worker (l, n, d) = (l', n', d')
            where
                l' = findLargest (\x -> (d*x-n)^2 < sq) [0..]
                n' = d * l' - n
                d' = div (sq - n' * n') d

sqrtFractionTest = [
    (0,0,1) @=? (head $ sqrtFraction 2),
    (1,1,1) @=? ((sqrtFraction 2) !! 1),
    (2,1,1) @=? ((sqrtFraction 2) !! 2),
    (2,1,1) @=? ((sqrtFraction 2) !! 3)
    ]

findLargest :: (a -> Bool) -> [a] -> a
findLargest test (x:x':xs) = if test x' 
    then findLargest test (x':xs)
    else x

findLargestTest = [
    8 @=? findLargest (\x -> (1*x-4)^2<23) [0..],
    1 @=? findLargest (\x -> (7*x-3)^2<23) [0..],
    1 @=? findLargest (\x -> (7*x-4)^2<23) [0..]
    ]

unitTests = map TestCase $
    findLargestTest ++
    compressTest ++
    sqrtFractionTest
    
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

