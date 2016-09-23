{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Using names.txt, a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?

import Common
import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))

type Names = String

problem0022 :: String -> Integer
problem0022 _ = 0

problem0022Test = map TestCase [
    ]

unitTests = problem0022Test

main = euler_main $ EulerFuncs {
    problem = problem0022,
    euler = problem0022 "problem_0022_names.txt",
    tests = unitTests,
    message = "Hello world: %d\n",
    defaults = [AdHoc "problem_0022_names.txt", Euler, UnitTest]
    }
