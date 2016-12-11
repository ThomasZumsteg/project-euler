{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}


import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Char (digitToInt)

data EulerArgs = 
    AdHoc { nths::[Int] }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

-- An irrational decimal fraction is created by concatenating the positive integers:
-- 0.12345678910>1<112131415161718192021...
-- It can be seen that the 12th digit of the fractional part is 1.
-- If dn represents the nth digit of the fractional part, find the value of the following expression.
-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

problem0040 :: [Int] -> [Int]
problem0040 = map (digitToInt . (!!) irrationalDecimal . pred)

irrationalDecimal :: String
irrationalDecimal = concat $ map show [1..]

irrationalDecimalTest = [
     "9" @=? (take 1 $ drop  8 irrationalDecimal),
    "11" @=? (take 2 $ drop 11 irrationalDecimal),
    "10" @=? (take 2 $ drop  9 irrationalDecimal),
     "1" @=? (take 1 $ drop 12 irrationalDecimal)]

unitTests = map TestCase $
    irrationalDecimalTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let digits = problem0040 nths
    printf "Product of digits is %d\n" (product digits)
    mapM_ (\(n,d) -> printf "%10d: %d\n" n d) (zip nths digits)
exec Euler = do
    let answer = product $ problem0040 $ map ((^) 10 ) [0..6]
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ nths = [1,10,100]}, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
