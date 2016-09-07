{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf (printf, PrintfArg)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Dates
import Data.Maybe (fromJust)
import Data.Either (isLeft) 
import Text.Parsec.Error (ParseError)

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

problem0019 :: (DateTime -> Bool) -> DateTime -> DateTime -> Int
problem0019 test start stop = length $ filter test dates
    where
        dates = takeWhile ((>) stop) $ iterate dayByDay start
        dayByDay = flip addInterval (Days 1)

firstSundayOfTheMonth :: DateTime -> Bool
firstSundayOfTheMonth date = (Sunday == dateWeekDay date) && (1 == day date)

data EulerArgs = 
    AdHoc { start :: String, stop :: String }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

type EitherDate = Either ParseError DateTime

printAnswer :: EitherDate -> EitherDate -> Either ParseError Int -> IO ()
printAnswer (Left s) _ (Left e) = do printf "Bad start date: %s\nError: %s\n" (show s) (show e)
printAnswer _ (Left s) (Left e) = do printf "Bad end date: %s\nError: %s\n" (show s) (show e)
printAnswer (Right start) (Right stop) (Right answer) = do 
    printf "Between %s and %s the first of the month is a Sunday %d times\n" (show start) (show stop) answer

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    now <- getCurrentDateTime
    let 
        startDate = parseDate (DateTime 1901 1 1 0 0 0) start
        stopDate = parseDate now stop
        answer = problem0019 firstSundayOfTheMonth <$> startDate <*> stopDate
    printAnswer startDate stopDate answer
exec Euler = do
    let 
        startDate = DateTime 1901 1 1 0 0 0
        stopDate = DateTime 2001 1 1 0 0 0 
        answer = problem0019 firstSundayOfTheMonth startDate stopDate 
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList $ []
    return ()

adHoc = AdHoc{ start = "today", stop = "1/1/2001" }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
