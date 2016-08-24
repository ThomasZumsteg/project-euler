{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf (printf, PrintfArg)
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
problem0017 start stop = sum $ map countLetters [start..stop]

countLetters :: (Integral a) => a -> Int
countLetters = length . filter isAlpha . fromJust . inEnglish . fromIntegral

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || 1000 < n = Nothing
  | n == 0 = Just "zero"
  | n == 1000 = Just "one thousand"
  | h == 0 && t == 0 && o /= 0 = Just $ one
  | h == 0 && t == 1 && o /= 0 = Just $ teen
  | h == 0 && t /= 0 && o == 0 = Just $ ten
  | h /= 0 && t == 0 && o == 0 = Just $ hundred
  | h == 0 && t /= 0 && o /= 0 = Just $ printf "%s-%s" ten one
  | h /= 0 && t == 1 && o /= 0 = Just $ printf "%s and %s" hundred teen
  | h /= 0 && t /= 0 && o == 0 = Just $ printf "%s and %s" hundred ten
  | h /= 0 && t == 0 && o /= 0 = Just $ printf "%s and %s" hundred one
  | otherwise = Just $ printf "%s and %s-%s" hundred ten one
  where
    (h:t:o:_) = map digitToInt $ printf "%03d" n
    one = ones !! o
    teen = teens !! o
    ten = tens !! t
    hundred = hundreds !! h

ones :: [String]
ones = [ "", "one", "two", "three", "four", "five", 
    "six", "seven", "eight", "nine" ]

teens :: [String]
teens = [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
    "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = [ "", "ten", "twenty", "thirty", "forty", "fifty",
    "sixty", "seventy", "eighty", "ninety" ]

hundreds :: [String]
hundreds = "": ( map (flip (++) " hundred") $ tail ones )

problem0017Test :: [Test]
problem0017Test = map TestCase [
    19 @=? problem0017 1 5
    ]

inEnglishTest :: [Test]
inEnglishTest = map TestCase [
    Nothing @=? inEnglish ( -1 ),
    Nothing @=? inEnglish 1001,
    "zero" @=? (fromJust $ inEnglish 0),
    "one thousand" @=? (fromJust $ inEnglish 1000),
    "one" @=? (fromJust $ inEnglish 1),
    "ten" @=? (fromJust $ inEnglish 10),
    "eleven" @=? (fromJust $ inEnglish 11),
    "one hundred" @=? (fromJust $ inEnglish 100),
    "three hundred and forty-two" @=? (fromJust $ inEnglish 342),
    "forty-three" @=? (fromJust $ inEnglish 43),
    "one hundred and ten" @=? (fromJust $ inEnglish 110),
    "one hundred and one" @=? (fromJust $ inEnglish 101),
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
