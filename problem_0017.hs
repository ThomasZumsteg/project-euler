{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf (printf)
import Test.HUnit ((@=?), runTestTT, Test(..))
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (IOMode( ReadMode), hGetContents, openFile)
import Data.Char (digitToInt, isAlpha)
import Data.Maybe (fromJust)

-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.


problem0017 :: (Integral a) => a -> a -> Int
problem0017 start stop = sum $ map (countLetters . fromJust . inEnglish) [start..stop]

countLetters :: String -> Int
countLetters = length . filter isAlpha

inEnglish :: Integral a => a -> Maybe String
inEnglish n
  | n == 0 = Just "zero"
  | n < 0 || (truncate (1e12::Double)) <= n = Nothing
  | otherwise =  Just $ unwords 
    $ map (\(num, mag) -> digits num ++ mag) magnitudes
  where
    magnitudes = reverse 
      $ filter ((/= 0) . fst) 
      $ zip (groups n) ["", " thousand", " million", " billion"]

groups :: Integral a => a -> [a]
groups 0 = []
groups g = re: groups quo
  where
    (quo, re) = divMod g 1000

digits :: Integral a => a -> String
digits n
  | 10 < n && n < 20 = teens n
  | otherwise = joinDigits (h,t,o)
  where
    (h, teen) = divMod n 100
    (t, o) = divMod teen 10

joinDigits :: Integral a => (a, a, a) -> String
joinDigits (0, 0, n) = ones n
joinDigits (0, t, 0) = tens t
joinDigits (h, 0, 0) = ones h ++ " hundred"
joinDigits (0, t, n) = tens t ++ "-" ++ ones n
joinDigits (h, t, 0) = ones h ++ " hundred " ++ tens t
joinDigits (h, t, n) = ones h ++ " hundred " ++ tens t ++ "-" ++ ones n

ones :: Integral a => a -> String
ones n = case n of 
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> ""

teens :: Integral a => a -> String
teens n = case n of
  11 -> "eleven"
  12 -> "twelve"
  13 -> "thirteen"
  14 -> "fourteen"
  15 -> "fifteen"
  16 -> "sixteen"
  17 -> "seventeen"
  18 -> "eighteen"
  19 -> "nineteen"
  _ -> ""

tens :: Integral a => a -> String
tens n = case n of
  1 -> "ten"
  2 -> "twenty"
  3 -> "thirty"
  4 -> "forty"
  5 -> "fifty"
  6 -> "sixty"
  7 -> "seventy"
  8 -> "eighty"
  9 -> "ninety"
  _ -> ""

problem0017Test :: [Test]
problem0017Test = map TestCase [
    19 @=? problem0017 1 5
    ]

inEnglishTest :: [Test]
inEnglishTest = map TestCase [
    "three hundred and forty-two" @=? (fromJust $ inEnglish 342),
    "one hundred and fifteen" @=? (fromJust $ inEnglish 115)
    ]

data EulerArgs = 
    AdHoc { start :: Integer, stop :: Integer }
    | Euler 
    | UnitTest
        deriving (Show, Data, Typeable)

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = problem0017 start stop
    printf "To make the words from %d to %d it takes %d letters\n" start stop answer
exec Euler = do
    let answer = problem0017 1 1000
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList $ problem0017Test ++ inEnglishTest
    return ()

adHoc = AdHoc{ start = 1, stop = 5 }
unittest = UnitTest{}
euler = Euler{}

main :: IO ()
main = do
    args <- cmdArgs $ modes [euler, unittest, adHoc]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
