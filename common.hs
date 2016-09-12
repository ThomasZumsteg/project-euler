{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Common where

import Text.Printf (printf, PrintfArg)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs  = 
    AdHoc Integer String
    | Euler Integer
    | UnitTest [Test]
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec (AdHoc problem message) = do
    let answer = problem
    printf message answer
exec (Euler problem) = do
    let answer = problem
    printf "Answer: %d\n" answer 
exec (UnitTest tests) = do 
    runTestTT $ TestList tests 
    return ()

euler_main :: [EulerArgs] -> IO ()
euler_main ms = do
    args <- cmdArgs $ modes ms
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start

