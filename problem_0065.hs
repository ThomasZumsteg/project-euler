{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

-- The square root of 2 can be written as an infinite continued fraction.
-- √2 = 1 + (1/(2 + (1/(2 + (1/(2 + (1/(2 + ...))))))))
-- The infinite continued fraction can be written, √2 = [1;(2)], (2) indicates that 2 repeats ad infinitum. In a similar way, √23 = [4;(1,3,1,8)].
-- It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations. Let us consider the convergents for √2.
-- 1 + (1/2) = (3/2)
-- 1 + (1/(2+(1/2))) = (7/5)
-- 1 + (1/(2+(1/(2+(1/2))))) = 17/12
-- 1 + (1/(2+(1/(2+(1/(2+(1/2))))))) = 41/29
-- Hence the sequence of the first ten convergents for √2 are:
-- 1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
-- What is most surprising is that the important mathematical constant,
-- e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
-- The first ten terms in the sequence of convergents for e are:
-- 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
-- The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
-- Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.

problem0065 :: Integer -> String -> Integer -> [Fraction]
problem0065 times operation value = case operation of
    "sqrt" -> take times $ iterator (sqrtFractionWorker value)
    "e" -> take times $ iterator eFractionWorker summer
    where
        magicFunc = error "Not Implemented"

type SqrtState = (Integer, Integer, Integer)
type Fraction = (Integer, Integer)

sqrtFractionWorker :: Integer -> [SqrtState]
sqrtFractionWorker sq (_, n, d) = (l, n', d')
    where
        l = findLargest (\x -> (d*x-n)^2 < sq) [0..]
        n' = d * l - n
        d' = div (sq - n' * n') d

sqrtFractionExpansionTest = [
    (1,4,7) @=? sqrtFractionWorker 23 (4,0,1),
    (2,1,1) @=? sqrtFractionWorker 2 (2,1,1),
    (1,1,2) @=? sqrtFractionWorker 3 (2,1,1),
    (1,1,2) @=? sqrtFractionWorker 3 (2,1,1)
    ]

iterator :: [a] -> (Fraction -> a -> Fraction) -> [Fraction]
iterator xs summer = error "Not Implemented"

eFractionWorker :: [Integer]
eFractionWorker = 2 : [
    if (1 == mod i 3) then (2 * div (i + 2) 3) else 1 
    | i <- [0..]]

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
    sqrtFractionExpansionTest

data Arg = Euler | UnitTest |
    AdHoc {value::Integer,times::Integer,oper::String} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = problem0065 100 "e" -1
        printf "Answer %s\n" answer
    exec AdHoc{..} = do
        let answer = problem0065 times oper value
        mapM_ (printf "%s\n" . show) answer 
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {times=10,oper="sqrt",value=2}]

