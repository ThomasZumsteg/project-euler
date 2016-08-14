{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

import Text.Printf (printf)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (IOMode( ReadMode), hGetContents, openFile)
import Data.Char (digitToInt)

problem0013 :: (Integral a, Show a) => Int -> [a] -> String
problem0013 digits = take digits . show . sum

getNumberList :: String -> IO [Integer]
getNumberList fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    return $ map (fromIntegral . read) $ lines contents

data EulerArgs = 
    AdHoc { 
        digits :: Int,
        fileName :: String }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    numberList <- getNumberList fileName 
    let answer = problem0013 digits numberList
    printf "The first %d digits is: %s\n" digits answer
exec Euler = do
    numberList <- getNumberList "problem_0013.txt"
    let answer = problem0013 10 numberList
    printf "Answer: %s\n" answer 
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
