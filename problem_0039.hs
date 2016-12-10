{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
-- {20,48,52}, {24,45,51}, {30,40,50}
-- For which value of p ≤ 1000, is the number of solutions maximised?

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.List (maximumBy)

data EulerArgs = 
    AdHoc { limit::Int }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

data Triple = Triple { 
    a::Int, 
    b::Int, 
    c::Int
    } deriving (Show, Eq)

problem0038 :: Int -> [Triple]
problem0038 lim = maximumBy numItems $ map triples [1..lim]
    where
        numItems x y = compare (length x) (length y)

triples :: Int -> [Triple]
triples p = [Triple a b c | 
    c <- [(quot (p + 1) 3)..(p - 2)],
    b <- [(quot (p - c + 1) 2)..(min c (p - c - 1))],
    let a = p - c - b,
    a^2 + b^2 == c^2 ]

triplesTest = [
    [Triple 30 40 50, Triple 24 45 51, Triple 20 48 52] @=? triples 120,
    True @=? (all (\t -> ( a t <= b t ) && ( b t <= c t )) $ triples 10)]

perimeter :: Triple -> Int
perimeter (Triple a b c) = a + b + c

unitTests = map TestCase $
    triplesTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let solutions = problem0038 limit
    printf "Less than %d the perimeter %d has %d unit solutions:\n" 
        limit (perimeter $ head solutions) (length solutions)
    mapM_ (\s -> printf "(%d, %d, %d)\n" (a s) (b s) (c s)) solutions
exec Euler = do
    let answer = perimeter $ head $ problem0038 1000
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ limit = 130 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
