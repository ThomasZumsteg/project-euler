{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs =
    Euler
    | AdHoc { file::String }
    | UnitTest
    deriving (Show, Data, Typeable)

data Solution = Solution { key::String, text::String, score::Double }
    deriving (Show)

problem0059 :: String -> Solution
problem0059 cipherText = error "Not Implemented"

splitOn :: String -> String -> [String]
splitOn sep text = error "Not Implemented"

dropWith' :: (Eq a) => [a] -> [a] -> [a]
dropWith' [] text = text
dropWith' tx text = if t /= tx then text else dropWith' tx $ drop l text 
    where
        l = length tx
        t = take l text

dropWith'Test = [
    "bcd" @=? dropWith' "a" "abcd",
    "cd" @=? dropWith' "ab" "abcd",
    "cd" @=? dropWith' "ab" "ababcd",
    "" @=? dropWith' "ab" "",
    "abcde" @=? dropWith' "ac" "abcde",
    "abcde" @=? dropWith' "" "abcde"]


unitTests = map TestCase $
    dropWith'Test

exec :: EulerArgs -> IO ()
exec Euler = do
    text <- readFile "problem_0059.txt"
    let answer = problem0059 text
    printf "Answer: %d\n" $ show answer
exec AdHoc{..} = do
    text <- readFile file
    let answer = problem0059 text
    return ()
exec UnitTest = do
    runTestTT $ TestList unitTests
    return ()
main :: IO ()
main = do
    args <- cmdArgs $ modes [
        Euler,
        AdHoc { file = "problem_0059.txt" },
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    printf "That took %s\n"  $ show $ diffUTCTime stop start
