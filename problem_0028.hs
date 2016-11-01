{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
-- It can be verified that the sum of the numbers on the diagonals is 101.
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
    
import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

data EulerArgs = 
    AdHoc { size::Integer }
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0028 :: Integer -> Integer
problem0028 limit = sum $ map last $ take (fromIntegral $ 2 * limit - 1) spiral

spiral :: [[Integer]]
spiral = [1] : (map convertState $ iterate incrementState initalState)
    where
        initalState = (SpiralState 2 2 1)
        convertState (SpiralState start diff _) = [start..(start + diff - 1)]

spiralTest = [
    [14,15,16,17] @=? (head $ drop 6 spiral),
    [10,11,12,13] @=? (head $ drop 5 spiral),
    [8,9] @=? (head $ drop 4 spiral),
    [6,7] @=? (head $ drop 3 spiral),
    [4,5] @=? (head $ drop 2 spiral),
    [2,3] @=? (head $ drop 1 spiral),
    [1] @=? head spiral]

data SpiralState = SpiralState {
    start :: Integer,
    diff :: Integer,
    side :: Integer
} deriving (Eq, Show)

incrementState :: SpiralState -> SpiralState
incrementState (SpiralState start diff side) 
    = SpiralState 
        (start + diff) 
        (diff + if (mod side 4) == 0 then 2 else 0)
        (succ side) 

incrementStateTest = [
    (SpiralState 8 6 5) @=? incrementState (SpiralState 4 4 4),
    (SpiralState 6 4 5) @=? incrementState (SpiralState 4 2 4),
    (SpiralState 3 4 5) @=? incrementState (SpiralState 1 2 4),
    (SpiralState 3 2 2) @=? incrementState (SpiralState 1 2 1)]

unitTests = map TestCase $
    spiralTest ++
    incrementStateTest

exec :: EulerArgs -> IO ()
exec AdHoc{..}= do
    let answer = problem0028 size
    printf "The sum of the diagonals of a %d x %d spiral is %d\n" size size answer
exec Euler = do
    let answer = problem0028 1001
    printf "Answer: %d\n" answer
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [AdHoc{ size=5 }, Euler, UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
