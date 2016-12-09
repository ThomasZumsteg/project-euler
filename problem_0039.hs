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
    AdHoc { limit::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

data Triple = Triple { 
    a::Integer, 
    b::Integer, 
    c::Integer 
    } deriving (Show, Eq)

problem0038 :: Integer -> (Integer, [Triple])
problem0038 lim = maximumBy sndLength $ map makeTuple [1..lim]
    where
        isRight (Triple a b c) = a^2 + b^2 == c^2
        makeTuple p = (p, filter isRight $ triples p)
        sndLength (_, x) (_, y) = compare (length x) (length y)

triples :: Integer -> [Triple]
triples p = [Triple a b c | 
    c <- [(ceiling $ (fromIntegral p) / 3)..(p - 2)],
    b <- [(ceiling $ (fromIntegral $ p - c) / 2)..(min c (p - c - 1))],
    let a = p - c - b ]

triplesTest = [
    [Triple 2 2 2, Triple 1 2 3, Triple 1 1 4] @=? triples 6,
    True @=? (all (\t -> ( a t <= b t ) && ( b t <= c t )) $ triples 10)]

unitTests = map TestCase $
    triplesTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let (perimeter, solutions) = problem0038 limit
    printf "Less than %d the perimeter %d has %d unit solutions:\n" 
        limit perimeter (length solutions)
    mapM_ (\s -> printf "(%d, %d, %d)\n" (a s) (b s) (c s)) solutions
exec Euler = do
    let answer = fst $ problem0038 1000
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
