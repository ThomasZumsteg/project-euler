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
problem0064 start stop = error "Not Implemented"

sqrtFractionExpansion :: Integer -> [Integer]
sqrtFractionExpansion n = error "Not Implemented"

sqrtFractionExpansionTest = [
    [1,2,2,2,2,2] @=? (take 6 $ sqrtFractionExpansion 2),
    [1,1,2,1,2,1] @=? (take 6 $ sqrtFractionExpansion 3),
    [2          ] @=? (take 6 $ sqrtFractionExpansion 4),
    [2,4,4,4,4,4] @=? (take 6 $ sqrtFractionExpansion 5)
    ]

unitTests = map TestCase $
    sqrtFractionExpansionTest

data Arg = Euler | UnitTest |
    AdHoc {start::Integer,stop::Integer} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = filter (\_ -> False) $ problem0064 2 10000
        printf "Answer %d\n" (length answer)
    exec AdHoc{..} = do
        let answer = problem0064 start stop
        mapM_ (printf "%s\n" . show) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {start=2,stop=13}]
