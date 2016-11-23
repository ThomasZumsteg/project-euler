{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

-- There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

-- If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Char (digitToInt)

data EulerArgs = 
    AdHoc
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

data Fraction = Fraction {
    numerator :: Integer,
    denominator :: Integer
    } deriving (Show, Eq)

problem0033 :: [(Integer, Integer)]
problem0033 = map (\(n,d) -> (toInteger n, toInteger d)) validFractions

validFractions :: [(Int, Int)]
validFractions = [ (n, d) | 
    n <- [10..100],
    d <- [ n..100],
    validFraction n d ]

gcdTest = [
    1 @=? gcd 10 1]

validFraction :: Int -> Int -> Bool
validFraction n d 
    | n == d = False
    | n < 10 || 100 <= n = False
    | d < 10 || 100 <= d = False
    | n' == '0' || n'' == '0' || d' == '0' || d'' == '0' = False
    | n' == d' = (d * digitToInt n'') == (n * digitToInt d'')
    | n'' == d'' = (d * digitToInt n') == (n * digitToInt d')
    | n' == d'' = (d * digitToInt n'') == (n * digitToInt d')
    | n'' == d' = (d * digitToInt n') == (n * digitToInt d'')
    | otherwise = False
    where
        (n':n'':[]) = show n
        (d':d'':[]) = show d

validFractionTest = [
    False @=? validFraction 33 52,
    False @=? validFraction 30 50,
    True @=? validFraction 49 98]

unitTests = map TestCase $
    validFractionTest

exec :: EulerArgs -> IO ()
exec AdHoc = do
    let answer = problem0033
    printf "List of fractions:\n" 
    mapM_ (\(n, d) -> printf "%d / %d\n" n d) answer
exec Euler = do
    let multiplyFactions (acc_n, acc_d) (n, d) = (acc_n * n, acc_d * d)
    let (answer_n, answer_d) = foldl1 multiplyFactions problem0033
    let common = gcd answer_n answer_d
    printf "Answer: %d\n" (div answer_d common)
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        AdHoc,
        Euler,
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
