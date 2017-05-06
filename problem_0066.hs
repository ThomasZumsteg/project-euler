{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

import Data.Maybe (mapMaybe)

-- Consider quadratic Diophantine equations of the form:
-- x² – Dy² = 1
-- For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.
-- It can be assumed that there are no solutions in positive integers when D is square.
-- By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
-- 32 – 2×22 = 1
-- 22 – 3×12 = 1
-- 92 – 5×42 = 1
-- 52 – 6×22 = 1
-- 82 – 7×32 = 1
-- Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
-- Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.

problem0066 :: [(Integer, Integer, Integer)]
problem0066 = error "not Implemented"

root :: Integer -> Integer -> Bool
root nth num = error "not Implemented"

rootTest = [
    True @=? root 2 4,
    True @=? root 3 27,
    False @=? root 3 4,
    False @=? root 2 27
    ]

func :: Integer -> Integer -> Maybe Integer
func d x = if r == 0 then Just q else Nothing
    where (q,r) = divMod (x - 1) d

funcTest = [
    Just 22 @=? func 2 32,
    Just 12 @=? func 3 22,
    [1802] @=? mapMaybe (func 13) [1..6492]
    ]

diophantine :: (Integer -> Maybe Integer) -> [Integer]
diophantine f = mapMaybe f [1..]

unitTests = map TestCase $
    rootTest ++
    funcTest

data Arg = Euler | UnitTest |
    AdHoc {times :: Int} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let (answer, _, _) = minimum $ take 1000 $ problem0066
        printf "Answer %d\n" answer
    exec AdHoc{..} = do
        let answer = take times $ problem0066 
        mapM_ (printf "%s\n" . show) answer 
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {times=10}]

