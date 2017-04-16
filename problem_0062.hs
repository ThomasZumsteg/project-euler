{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

-- The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
-- Find the smallest cube for which exactly five permutations of its digits are cube.

problem0062 :: Integer -> Int -> [[Integer]]
problem0062 exp perms = error "Not Impelemented"

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = concatMap addHead [splitAt (i - 1) xs | i <- [1..(length xs)]]
    where
        addHead (ps,(q:qs)) = map (q:) $ permutations (ps ++ qs)

permutationsTest = [
    [""] @=? permutations "",
    ["1"] @=? permutations "1",
    ["12","21"] @=? permutations "12",
    ["123","132", "213", "231", "312", "321"] @=? permutations "123"
    ]

unitTests = map TestCase $
    permutationsTest
    

data Arg = Euler | UnitTest |
    AdHoc {exponent::Integer, numPermutations::Int} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = head $ problem0062 3 5
        printf "Answer: %d\n" (sum answer)
    exec AdHoc{..} = do
        let answer = problem0062 exponent numPermutations
        mapM_ (printf "%s\n" . show) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {
    exponent=3, numPermutations=3}]
