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

problem0066 :: [(Integer, Integer, Integer)]
problem0066 = [(x,d,y) | 
    d <- [0..], isNothing $ integerRoot 2 d,
    let (x, y) = head $ [(a, fromJust b) | a <- [2..], let b = func d a, isJust b]]

-- √((x² - 1) / d) = y
func :: Integer -> Integer -> Maybe Integer
func d x = if r == 0 then integerRoot 2 q else Nothing
    where (q, r) = divMod (x * x - 1) d

funcTest = [
    Just 2 @=? func 2 3,
    Just 1 @=? func 3 2,
    [180] @=? mapMaybe (func 13) [2..649]
    ]

integerRoot :: Integer -> Integer -> Maybe Integer
integerRoot nth num = if (est ^ nth) == num then Just est else Nothing
    where
        est = round $ (fromInteger num) ** (1 / (fromInteger nth))

integerRootTest = [
    Just 2 @=? integerRoot 2 4,
    Nothing @=? integerRoot 2 5,
    Just 2 @=? integerRoot 3 8
    ]

diophantine :: (Integer -> Maybe Integer) -> [Integer]
diophantine f = mapMaybe f [2..]

diophantineTest = [
    [2,12,70,408,2378] @=? (take 5 $ diophantine (func 2)),
    [1,4,15,56,209] @=? (take 5 $ diophantine (func 3)),
    [4,72,1292,23184,416020] @=? (take 5 $ diophantine (func 5)),
    [2,20,198,1960,19402] @=? (take 5 $ diophantine (func 6)),
    [3,48,765,12192,194307] @=? (take 5 $ diophantine (func 7)),
    [180] @=? (take 1 $ diophantine (func 13))
    ]


unitTests = map TestCase $
    integerRootTest ++
    funcTest ++
    diophantineTest

data Arg = Euler | UnitTest |
    AdHoc {start :: Int, stop :: Int} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let (answer, _, _) = minimum $ take 1000 $ problem0066
        printf "Answer %d\n" answer
    exec AdHoc{..} = do
        let answer = drop start $ take stop $ problem0066 
        mapM_ (\(x,d,y) -> printf "%d² - %dx%d²=1\n" x d y) answer 
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {start=2, stop=10}]

