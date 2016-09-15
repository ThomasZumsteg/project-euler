{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Common where

import Text.Printf (printf, PrintfArg)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Map
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs a = AdHoc a | Euler | UnitTest
    deriving (Show, Data, Typeable)

data EulerFuncs a = EulerFuncs {
    problem :: (a -> Integer),
    euler :: Integer,
    tests :: [Test],
    message :: String,
    defaults :: [EulerArgs a]
    }
        
exec :: EulerFuncs a -> EulerArgs a -> IO ()
exec funcs (AdHoc args)= do
    let answer = (problem funcs) args
    printf (message funcs) answer
exec funcs Euler = do
    let answer = (euler funcs)
    printf "Answer: %d\n" answer 
exec funcs UnitTest = do 
    runTestTT $ TestList $ tests funcs
    return ()

euler_main :: EulerFuncs a -> IO ()
euler_main funcs = do
    args <- cmdArgs $ modes $ defaults funcs
    start <- getCurrentTime
    exec funcs args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
