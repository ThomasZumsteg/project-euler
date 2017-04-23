{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

-- The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
-- How many n-digit positive integers exist which are also an nth power?

problem0063 :: Int
problem0063 = length $ concat $ takeWhile (/=[]) $ map digitPower [1..]

digits :: Integer -> Int
digits n = floor (log (fromIntegral n) / log 10) + 1

slice :: (a -> Bool) -> [a] -> ([a],[a],[a])
slice test xs = (drops, pass, past)
    where
        (drops, xs') = span (not . test) xs
        (pass, past) = span test xs'

digitPower :: Integer -> [Integer]
digitPower d = second $ slice digitEqualPower [1..]
    where
        second (_, m, _) = m
        power = flip (^) d
        digitEqualPower n = (digits $ power n) == (fromIntegral d)

testDigitPower = [
    [1..9] @=? digitPower 1,
    [4..9] @=? digitPower 2,
    [5..10] @=? digitPower 3 
    ]

unitTests = map TestCase $
    testDigitPower

data Arg = Euler | UnitTest |
    AdHoc {start::Integer, stop::Integer} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = problem0063
        printf "Answer %d" answer 
    exec AdHoc{..} = do
        let answer = concatMap digitPower [1..]
        mapM_ (printf "%s\n" . show) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {start=5 , stop=9}]
