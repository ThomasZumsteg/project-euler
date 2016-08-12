{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

import Text.Printf (printf)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (IOMode( ReadMode), hGetContents, openFile)
import Data.Char (digitToInt)

problem0013 :: (Integral a) => a -> [a] -> a
problem0013 digits = foldl (\n total -> rem (n + total) (10 ^ digits)) 0 

lastN :: Int -> [a] -> [a]
lastN l items = drop firstN items
    where
        firstN = (length items) - l

readFirstNDigits :: String -> Int -> [Int]
readFirstNDigits fileName _ = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let numberList = map (fromIntegral . read . lastN digits) $ lines contents
    return numberList

data EulerArgs = 
    AdHoc { 
        digits :: Int,
        fileName :: String }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let numberList = readFirstNDigits "problem_0013.txt" 10
    printf "The first %d digits is: %d\n" digits (problem0013 digits numberList)
exec Euler = do
    -- let result = problem0013 10 numberList 
    printf "Answer: %d\n" (problem0013 (10::Integer) [123, 456, 789])
exec UnitTest = do 
    runTestTT $ TestList $ []
    return ()

adHoc = AdHoc{ fileName = "problem_0013.txt", digits = 10 }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
