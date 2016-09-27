{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Using names.txt, a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf, PrintfArg)
import System.Console.CmdArgs
import qualified Data.Map as M
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Char (ord)
import Data.List (sort)

data EulerArgs = 
    AdHoc { file_name::String }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

type Name = String
type NameData = String

problem0022 :: NameData -> Integer
problem0022 = foldl nameScore 0 . zip [1..] . sort . splitNames ""
    where
        nameScore total (index, name) = total + index * (scoreName name)

problem0022Test = map TestCase [
    68 @=? problem0022 "\"BOB\",\"ALICE\"",
     0 @=? problem0022 "",
    19 @=? problem0022 "\"BOB\""
    ]

splitNames :: Name -> NameData -> [Name]
splitNames _ "" = []
splitNames name (c:cs) 
    | 'A' <= c && c <= 'Z' = splitNames (name ++ [c]) cs
    | name == "" = splitNames "" cs
    | otherwise = name : splitNames "" cs 

splitNamesTest = map TestCase [
    ["SAM"] @=? splitNames "" "\"SAM\"",
    ["SAM", "JACK"] @=? splitNames "" "\"SAM\",\"JACK\"",
    ["ALICE","BOB","EVE"] @=? splitNames "" "\"ALICE\",\"BOB\",\"EVE\""
    ]

scoreName :: Name -> Integer
scoreName = toInteger . sum . map letterValue 
    where
        letterValue l = (ord l) - (ord 'A') + 1

scoreNameTest = map TestCase [
    1 @=? scoreName "A",
    2 @=? scoreName "B",
    3 @=? scoreName "AB",
   19 @=? scoreName "BOB",
   30 @=? scoreName "ALICE",
   32 @=? scoreName "EVE",
   53 @=? scoreName "COLIN"
    ]

unitTests = problem0022Test ++
    scoreNameTest ++
    splitNamesTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    names <- readFile file_name 
    let answer = problem0022 names
    printf  "\"%s\" has a score of: %d\n" file_name answer
exec Euler = do
    names <- readFile "problem_0022_names.txt"
    let answer = problem0022 names
    printf "Answer: %d\n" answer 
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ file_name = "problem_0022_names.txt" }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
