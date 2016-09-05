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
--
-- let firstSundays = takeWhile (\d -> d < (DateTime 2001 1 1 0 0 0)) $ filter (\d -> day d == 1) $ iterate (flip addInterval (Days 7)) (DateTime 1901 1 6 0 0 0)

data Date = Date {
    year :: Int,
    month :: Month,
    day :: Int
    } deriving (Eq, Show)

instance Ord Date where
    compare (Date year1 month1 date1) (Date year2 month2 date2)
        | year1 /= year2 = compare year1 year2
        | month1 /= month2 = compare month1 month2
        | otherwise = compare date1 date2

tomorrow :: Date -> Date
tomorrow (Date year month date) 
    | daysInMonth year month < succ date = if month == DEC 
        then Date (succ year) JAN 1
        else Date year (succ month) 1
    | otherwise = Date year month (succ date)

yesterday :: Date -> Date
yesterday (Date year month date) 
    | pred date <= 0 = if month == JAN
        then Date (pred year) DEC 31
        else Date year (pred month) (daysInMonth year (pred month))
    | otherwise = Date year month (pred date)

type Year = Int
data Month = JAN | FEB | MAR | APR | MAY | JUN | JUL | AUG | SEP | OCT | NOV | DEC
    deriving (Eq, Show, Enum, Ord)
type Day = Int

data WeekDay = MON | TUES | WED | THURS | FRI | SAT | SUN
    deriving (Eq, Show, Enum)

problem0019 :: Date -> Date -> (Date -> Bool) -> Int
problem0019 start stop test = length $ filter test dates
    where
        dates = dateRange start stop

dateRange :: Date -> Date -> [Date]
dateRange start stop 
    | stop <= start = []
    | otherwise = start : dateRange (tomorrow start) stop 

firstSundayOfTheMonth :: Date -> Bool
firstSundayOfTheMonth date = (SUN == weekDay date) && (day date == 1)

parseDate :: String -> Maybe Date
parseDate _ = Just firstMonday

daysInMonth :: Year -> Month -> Int
daysInMonth year month 
    | month `elem` [SEP, APR, JUN, NOV] = 30
    | month == FEB = if leapYear year then 29 else 28
    | otherwise = 31

leapYear :: Year -> Bool
leapYear year = (mod year 4 == 0) && (mod year 100 /= 0 || mod year 400 == 0)

weekDay :: Date -> WeekDay
weekDay d = toEnum (mod diff 7)
    where
        diff = daysBetween firstMonday d

daysBetween :: Date -> Date -> Int
daysBetween start stop = length $ dateRange start stop

firstMonday :: Date
firstMonday = Date 1900 JAN 1

daysInMonthTest :: [ Test ]
daysInMonthTest = map TestCase [
    31 @=? daysInMonth 1900 JAN,
    28 @=? daysInMonth 1900 FEB,
    28 @=? daysInMonth 1901 FEB,
    29 @=? daysInMonth 2000 FEB,
    28 @=? daysInMonth 2001 FEB,
    31 @=? daysInMonth 1900 MAR,
    30 @=? daysInMonth 1900 APR,
    31 @=? daysInMonth 1900 MAY,
    30 @=? daysInMonth 1900 JUN,
    31 @=? daysInMonth 1900 JUL,
    31 @=? daysInMonth 1900 AUG,
    30 @=? daysInMonth 1900 SEP,
    31 @=? daysInMonth 1900 OCT,
    30 @=? daysInMonth 1900 NOV,
    31 @=? daysInMonth 1900 DEC
    ]

tomorrowTest :: [Test] 
tomorrowTest = map TestCase [
    (Date 1900 JAN 2) @=? tomorrow (Date 1900 JAN 1),
    (Date 1900 FEB 1) @=? tomorrow (Date 1900 JAN 31),
    (Date 1901 JAN 1) @=? tomorrow (Date 1900 DEC 31)
    ]

yesterdayTest :: [Test]
yesterdayTest = map TestCase [
    (Date 1900 JAN 1) @=? yesterday (Date 1900 JAN 2),
    (Date 1900 JAN 31) @=? yesterday (Date 1900 FEB 1),
    (Date 1900 DEC 31) @=? yesterday (Date 1901 DEC 31)
    ]

ordDateTest :: [Test]
ordDateTest = map TestCase [
    EQ @=? compare (Date 1900 JAN 1) (Date 1900 JAN 1),
    LT @=? compare (Date 1900 JAN 1) (Date 1900 JAN 2),
    LT @=? compare (Date 1900 JAN 1) (Date 1900 FEB 1),
    LT @=? compare (Date 1900 JAN 1) (Date 1901 JAN 1)
    ]

dateRangeTest :: [Test]
dateRangeTest = map TestCase [
    dates @=? dateRange (Date 1900 JAN 1) (Date 1900 FEB 1),
    [a, b, c] @=? dateRange (Date 1900 JAN 1) (Date 1900 JAN 4)
    ]
    where
        a = Date 1900 JAN 1
        b = Date 1900 JAN 2
        c = Date 1900 JAN 3
        dates = take 31 $ iterate tomorrow a

daysBetweenTest :: [Test]
daysBetweenTest = map TestCase [
    365 @=? daysBetween (Date 1900 JAN 1) (Date 1901 JAN 1),
    31 @=? daysBetween (Date 1900 JAN 1) (Date 1900 FEB 1), 
    1 @=? daysBetween (Date 1900 JAN 1) (Date 1900 JAN 2),
    0 @=? daysBetween firstMonday firstMonday
    ]

weekDayTest :: [Test]
weekDayTest = map TestCase [
    THURS @=? weekDay (Date 1900 JAN 4),
    WED @=? weekDay (Date 1900 JAN 3),
    TUES @=? weekDay (Date 1900 JAN 2),
    MON @=? weekDay firstMonday
    ]

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
        startDate = firstMonday
        stopDate = Date 2001 JAN 1 
        answer = problem0019 startDate stopDate firstSundayOfTheMonth
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList $ daysInMonthTest ++ tomorrowTest ++ ordDateTest 
        ++ dateRangeTest ++ daysBetweenTest ++ weekDayTest
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
