{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

import Data.Maybe (mapMaybe, fromJust, isJust, isNothing)
import Data.List (elemIndex)
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
    let (x, y) = sumFractionExpantion $ compressedSqrtExpansion d]
    where
        squares = [n*n | n <- [1..stop]]

sumFractionExpantion :: ([Integer], [Integer]) -> (Integer, Integer)
sumFractionExpantion (xs, ys@(y:_)) = foldr worker (0,1) (xs ++ ys ++ [y])
    where
        worker n (m, e) = (e * n + m, m)

compressedSqrtExpansion :: Integer -> ([Integer],[Integer])
compressedSqrtExpansion = fsts . compress . sqrtFraction
    where
        fsts (xs, ys) = (map fst xs, map fst ys)
        fst (x, _, _) = x

compressedSqrtExpansionTest = [
    ([1],[2]) @=? compressedSqrtExpansion 2,
    ([1],[1,2]) @=? compressedSqrtExpansion 3,
    ([2],[4]) @=? compressedSqrtExpansion 5,
    ([2],[2,4]) @=? compressedSqrtExpansion 6
    ]

compress :: (Eq a) => [a] -> ([a],[a])
compress xs = worker xs []
    where
        worker (x:xs) ys = case elemIndex x ys of
            Just i -> splitAt (length ys - i - 1) (reverse ys)
            Nothing -> worker xs (x:ys)

compressTest = [
    ([], [1]) @=? (compress $ repeat 1),
    ([], [2, 3]) @=? (compress $ cycle [2, 3]),
    ([(1,1,1)],[(2,1,1)]) @=? (compress $ sqrtFraction 2),
    ([(1,1,2)],[(1,1,1),(2,1,2)]) @=? (compress $ sqrtFraction 3),
    ([(2,2,1)],[(4,2,1)]) @=? (compress $ sqrtFraction 5),
    ([(2,2,2)],[(2,2,1),(4,2,2)]) @=? (compress $ sqrtFraction 6),
    ([(4,4,7)],[(1,3,2),(3,3,7),(1,4,1),(8,4,7)]) @=? (compress $ sqrtFraction 23)
    ]

sqrtFraction :: Integer -> [(Integer, Integer, Integer)]
sqrtFraction sq = tail $ iterate worker (0, 0, 1)
    where
        worker (l, n, d) = (l', n', d')
            where
                l' = findLargest (\x -> (d*x-n)^2 < sq) [0..]
                n' = d * l' - n
                d' = div (sq - n' * n') d

sqrtFractionTest = [
    (1,1,1) @=? ((sqrtFraction 2) !! 0),
    (2,1,1) @=? ((sqrtFraction 2) !! 1),
    (2,1,1) @=? ((sqrtFraction 2) !! 2)
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
    sqrtFractionTest ++
    compressedSqrtExpansionTest
    
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

