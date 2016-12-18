{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.
-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf, PrintfArg)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Char (ord, toUpper)

data EulerArgs = 
    AdHoc { file_name::String }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0042 :: [String] -> [String]
problem0042 = filter (isTriangle . wordScore)

isTriangle :: Integer -> Bool
isTriangle 0 = False
isTriangle n = isPerfectSquare (8 * n + 1)

isTriangleTest = [
    False @=? isTriangle 2,
    False @=? isTriangle 0,
    True @=? isTriangle 1,
    True @=? isTriangle 3,
    True @=? isTriangle 55]

isPerfectSquare :: Integer -> Bool
isPerfectSquare n = 0 == remainer
    where
        remainer = head $ dropWhile (>0) $ scanl (-) n [1,3..] 

isPerfectSquareTest = [
    True @=? isPerfectSquare 16,
    True @=? isPerfectSquare 0,
    False @=? isPerfectSquare 10,
    True @=? isPerfectSquare 1]

wordScore :: String -> Integer
wordScore = sum . map lettterScore 
    where
        lettterScore c = case toInteger ((ord $ toUpper c) - ord 'A' + 1) of
            s | 0 < s && s < 27 -> s
            otherwise -> 0

wordScoreTest = [
    26 @=? wordScore "z",
    26 @=? wordScore "Z",
     3 @=? wordScore "AAA",
    55 @=? wordScore "SKY"]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen test chars = case dropWhile test chars of
    "" -> []
    chars' -> word : wordsWhen test chars''
        where (word, chars'') = break test chars'

unitTests = map TestCase $
    wordScoreTest ++
    isPerfectSquareTest ++
    isTriangleTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    text <- readFile file_name 
    let answer = problem0042 $ wordsWhen (==',') text
    printf  "Reading from %s:\n" file_name
    mapM_ (\word -> printf "%s: %d\n" word (wordScore word)) answer
    printf "Total: %d\n" (length answer)
exec Euler = do
    text <- readFile "problem_0042.txt"
    let answer = length $ problem0042 $ wordsWhen ((==) ',') text
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ file_name = "problem_0042.txt" }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
