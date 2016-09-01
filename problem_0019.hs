{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf (printf, PrintfArg)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Maybe (fromJust)

-- You are given the following information, but you may prefer to do some research for yourself.

--     1 Jan 1900 was a Monday.
--     Thirty days has September,
--     April, June and November.
--     All the rest have thirty-one,
--     Saving February alone,
--     Which has twenty-eight, rain or shine.
--     And on leap years, twenty-nine.
--     A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

data Date = Date {
    year :: Int,
    month :: Month,
    day :: Int
    } deriving (Eq, Show)

data WeekDay = Mon | Tues | Wed | Thurs | Fri | Sat | Sun
    deriving (Eq, Show, Enum)

data Month = JAN | FEB | MAR | APR | MAY | JUN | JUL | AUG | SEP | OCT | NOV | DEC
    deriving (Eq, Show, Enum)

problem0019 :: Date -> Date -> (Date -> Bool) -> Int
problem0019 start stop test = length $ filter test dates
    where
        dates = dateRange start stop

dateRange :: Date -> Date -> [Date]
dateRange start stop = []

firstSundayOfTheMonth :: Date -> Bool
firstSundayOfTheMonth _ = True

parseDate :: String -> Maybe Date
parseDate _ = Just $ Date 1900 JAN 1

data EulerArgs = 
    AdHoc { start :: String, stop :: String }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let 
        startDate = fromJust $ parseDate start
        stopDate = fromJust $ parseDate stop
        answer = problem0019 startDate stopDate firstSundayOfTheMonth
    printf "Sunday is the first day of the month between %s and %s, %d times\n" start stop answer
exec Euler = do
    let 
        startDate = Date 1900 JAN 1
        stopDate = Date 2000 DEC 31 
        answer = problem0019 startDate stopDate firstSundayOfTheMonth
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList $ []
    return ()

adHoc = AdHoc{ start = "", stop = "" }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
