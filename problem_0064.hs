{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

-- All square roots are periodic when written as continued fractions and can be written in the form:
-- √N = a0 + (1 / a1 + (1 / a2 + (1 / a3 + ...

-- For example, let us consider √23:
-- √23 = 4 + √23 — 4 = 4 + 1 = 4 + (1 / (1 / √23—4)) = 4 + (1 / (1 / ((√23 – 3)/ 7)))

-- If we continue we would get the following expansion:
-- √23 = 4 + (1 / (1 + (1 / (3 + (1 / (1 + 1 / 8 + ...

-- The process can be summarised as follows:
-- a0 = 4, 1/(√23—4) = (√23+4)/7 = 1 + (√23—3)/7
-- a1 = 1, 7/(√23—3) =  7(√23+3)/14 = 3 + (√23—3)/2
-- a2 = 3, 2/(√23—3) = 2(√23+3)/14 = 1 + (√23—4)/7
-- a3 = 1, 7/(√23—4) = 7(√23+4)/7 = 8 + (√23—4)
-- a4 = 8, 1/(√23—4) = (√23+4)/7 = 1 + (√23—3)/7
-- a5 = 1, 7/(√23—3) = 7(√23+3)/14 = 3 + (√23—3)/2
-- a6 = 3, 2/(√23—3) = 2(√23+3)/14 = 1 + (√23—4)/7
-- a7 = 1, 7/(√23—4) = 7(√23+4)/7 = = 8 + (√23—4)

-- It can be seen that the sequence is repeating. For conciseness, we use the notation √23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.

-- The first ten continued fraction representations of (irrational) square roots are:

-- √2=[1;(2)], period=1
-- √3=[1;(1,2)], period=2
-- √5=[2;(4)], period=1
-- √6=[2;(2,4)], period=2
-- √7=[2;(1,1,1,4)], period=4
-- √8=[2;(1,4)], period=2
-- √10=[3;(6)], period=1
-- √11=[3;(3,6)], period=2
-- √12= [3;(2,6)], period=2
-- √13=[3;(1,1,1,1,6)], period=5

-- Exactly four continued fractions, for N ≤ 13, have an odd period.

-- How many continued fractions for N ≤ 10000 have an odd period?

problem0064 :: Integer -> Integer -> [(Integer,[Integer])]
problem0064 start stop = map (fsts . compress . sqrtFractionExpansion) [start..stop]
    where
        fsts (x, xs) = (fst x, map fst xs)
        fst (x, _, _) = x

compress :: (Eq a) => [a] -> (a,[a])
compress (x:x':xs) = (x, x' : takeWhile (/=x') xs)

compressTest = [
    (1, [1]) @=? (compress $ repeat 1),
    (2, [3, 2]) @=? (compress $ cycle [2, 3])
    ]

-- find the largest intetger x such that 0 < (0+√23)/1 - x
--     (0+√23)/1 < x
--     23 <  x²
--     23 <  0 : 0
--     23 <  1 : 1
--     23 <  4 : 2
--     23 <  9 : 3
--     23 < 16 : 4 *
--     23 < 25 : 5
-- √23 = 4+(0+√23)/1-4 ; l (4) findLargest
--     = 4+(1/(1/(√23-4)))
--     = 4+(1/((4+√23)/(23-16))) ; n' (4) = d (1) * l (4) - n (0)
--     = 4+(1/((4+√23)/7)) ; d' (7) = (rt (23) - n'² (16)) / d (1)
--     find the largest integer x such that 0 < (4+√23)/7 - x
--     7x < (4+√23)
--     (7x-4)² < 23
--     16 < 23 ; 0
--      9 < 23 ; 1 *
--    100 < 23 ; 2
-- 4+√23 = 1+(4+√23)/7-1 ; l (1) findLargest
--       = 1+(4+√23-7)/7
--       = 1+(√23-3)/7 ; (3) n' = d (7) * l (1) - n (4)
--       = 1+1/(7/(√23-3))
--       = 1+1/(7(√23+3)/(23-9))
--       = 1+1/((√23+3)/2) ; d' = (rt - n'²) / d
-- find the largest integer x such that 0 < (3+√23)/2 - x
--       2x < 3+√23
--       (2x-3)² < 23
--       9 < 23 ; 0
--       1 < 23 ; 1
--       1 < 23 ; 2 *
--       64 < 23 ; 3
-- (3+√23)/2 = 3+(3+√23)/2-3 ; l (3) = findLargest
--           = 3+(3+√23-6)/2 ; n' (3) = (d (2) * l (3) - n (3)) 
--           = 3+(√23-3)/2
--           = 3+1/(2/(√23-3)) ; d' (7) = (rt - n'^2) / d
--           = 3+1/(2(√23+3)/(23-9)) ; 
--           = 3+1/((√23+3)/7)
-- find the largest integer x such that 0 < (√23+3)/7 - x ; (d*x-n)^2<rt
--           (7x-3)² < 23
--           9 < 23 ; 0
--           16 < 23 ; 1 *
--           121 < 23 ; 2
-- (3+√23)/7 = 1+(3+√23)/7-1 ; Find the largest (1)
--           = 1+(3+√23-7)/7 ; Multiply by the denominator
--           = 1+(√23-4)/7 ; Subtract denominator from the numberator to make
--           the next numberator
--           = 1+(1/(7/(√23-4))) ; from 23 subtract the next numerator
--           = 1+(1/((√23+4)/1)) ; 
-- find the largest integer x such that 0 < (√23+4)/1 - x
--           (x-4)² < 23
--           16 < 23 ; 0
--            9 < 23 ; 1
--            4 < 23 ; 2
--            1 < 23 ; 3
--            0 < 23 ; 4
--            1 < 23 ; 5
--            4 < 23 ; 6
--            9 < 23 ; 7
--           16 < 23 ; 8 *
--           25 < 23 ; 9
-- (4+√23)/1 = 8+(4+√23)/1-8
--           = 8+(4+√23-8)/1
--           = 8+(√23-4)/1
--           = 8+(1/(1/(√23-4)))
--           = 8+(1/((√23+4)/(23-16)))
--           = 8+(1/((√23+4)/7))
sqrtFractionExpansion :: Integer -> [(Integer,Integer,Integer)]
sqrtFractionExpansion sq = worker 0 1
    where
        worker n d = (l, n, d) : if l == 0 then [] else worker n' d'
            where
                l = findLargest (\x -> (d*x-n)^2 < sq) [0..]
                n' = d * l - n
                d' = div (sq - n' * n') d

sqrtFractionExpansionTest = [
    [(4,0,1),(1,4,7),(3,3,2),(1,3,7),(8,4,1),(1,4,7)]
        @=? (take 6 $ sqrtFractionExpansion 23),
    [(1,0,1),(2,1,1),(2,1,1),(2,1,1),(2,1,1),(2,1,1)]
        @=? (take 6 $ sqrtFractionExpansion 2),
    [(1,0,1),(1,1,2),(2,1,1),(1,1,2),(2,1,1),(1,1,2)]
        @=? (take 6 $ sqrtFractionExpansion 3),
    [(1,0,1),(0,1,3)]
        @=? (take 6 $ sqrtFractionExpansion 4),
    [(2,0,1),(4,2,1),(4,2,1),(4,2,1),(4,2,1),(4,2,1)]
        @=? (take 6 $ sqrtFractionExpansion 5)
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
    sqrtFractionExpansionTest ++
    compressTest

data Arg = Euler | UnitTest |
    AdHoc {start::Integer,stop::Integer} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = filter (\(_, c) -> (odd $ length c) && (c /= [0])) $ 
                problem0064 2 10000
        printf "Answer %d\n" (length answer)
    exec AdHoc{..} = do
        let answer = problem0064 start stop
        mapM_ (printf "%s\n" . show) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {start=2,stop=13}]
