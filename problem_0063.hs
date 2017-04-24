{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

-- The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
-- How many n-digit positive integers exist which are also an nth power?

problem0063 :: [[Integer]]
problem0063 = takeWhile (/=[]) $ map digitPower [1..]

digits :: Integer -> Int
digits n = length $ show n

slice :: (Eq a) => (a -> Bool) -> [a] -> ([a],[a],[a])
slice test xs = (drops, pass, past)
    where
        (drops, xs') = span (not . test) xs
        (pass, past) = span test xs'

digitPower :: Integer -> [Integer]
digitPower d = second $ slice digitEqualPower [1..10]
    where
        second (_, m, _) = m
        power = flip (^) d
        digitEqualPower n = (digits $ power n) == (fromIntegral d)

testDigitPower = [
    [1..9] @=? digitPower 1,
    [4..9] @=? digitPower 2,
    [5..9] @=? digitPower 3,
    [] @=? digitPower 22
    ]

unitTests = map TestCase $
    testDigitPower

data Arg = Euler | UnitTest |
    AdHoc {start::Integer, stop::Integer} 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = length $ concat $ takeWhile (/=[]) problem0063
        printf "Answer %d\n" answer 
    exec AdHoc{..} = do
        let answer = map (\n -> (n, digitPower n, map (flip (^) n) $ digitPower n)) [start..stop]
        mapM_ (\(n, num, ans) -> printf "%d: %s %s\n" n (show num) (show ans)) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc {start=1 , stop=25}]
