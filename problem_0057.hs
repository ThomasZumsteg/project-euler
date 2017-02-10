{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs =
    Euler
    | AdHoc { limit::Int }
    | UnitTest
    deriving (Show, Data, Typeable)

-- It is possible to show that the square root of two can be expressed as an infinite continued fraction.
--     √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
-- By expanding this for the first four iterations, we get:
--     1 + 1/2 = 3/2 = 1.5
--     1 + 1/(2 + 1/2) = 7/5 = 1.4
--     1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
--     1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
-- The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
-- In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

data Fraction = Fraction { 
    numerator::Integer,
    denominator::Integer }
    deriving (Show, Eq)

problem0056 :: Int -> [Fraction]
problem0056 limit = filter moreDigits $ take limit continuous
    where
        moreDigits (Fraction n d) = (length $ show n) > (length $ show d)

continuous :: [Fraction]
continuous = iterate itr (Fraction 1 1)
    where
        itr (Fraction n d) = Fraction (n + 2 * d) (n + d)

continuousTest = [
    Fraction 41 29 @=? (head $ drop 4 continuous),
    Fraction 17 12 @=? (head $ drop 3 continuous),
    Fraction  7  5 @=? (head $ drop 2 continuous),
    Fraction  3  2 @=? (head $ drop 1 continuous)]


unitTests = map TestCase $
    continuousTest

exec :: EulerArgs -> IO ()
exec Euler = do
    let  answer = length $ problem0056 1000
    printf "Answer: %d\n" answer
exec AdHoc{..} = do
    let  answer = zip ([1..]::[Integer]) $ problem0056 limit
    mapM_ (\(i, Fraction n d) -> printf "%d: %d/%d\n" i n d) answer
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc {limit = 10},
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
