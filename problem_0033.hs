{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

-- There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

-- If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

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
problem0033 = validFractions

validFractions :: [(Integer, Integer)]
validFractions = [ (n, d) | 
    n <- [10..100],
    d <- [ n..100],
    validFraction n d ]

validFraction :: Integer -> Integer -> Bool
validFraction n d 
    | 0 == mod n 10 = False
    | 0 == mod d 10 = False
    | otherwise = (n * d') == (n' * d)
    where
        n' = div n 10
        d' = mod d 10

validFractionTest = [
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
    let answer = problem0033
    printf "Answer: %s\n" (show answer)
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
