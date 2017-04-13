{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Set as Set
import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.List (sort, tails, findIndices)

import Common (exec, EulerArg, euler_main)

-- Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate (polygonal) numbers and are generated by the following formulae:
--     Triangle        P3,n=n(n+1)/2       1, 3,  6, 10, 15, ...
--     Square          P4,n=n2             1, 4,  9, 16, 25, ...
--     Pentagonal      P5,n=n(3n−1)/2      1, 5, 12, 22, 35, ...
--     Hexagonal       P6,n=n(2n−1)        1, 6, 15, 28, 45, ...
--     Heptagonal      P7,n=n(5n−3)/2      1, 7, 18, 34, 55, ...
--     Octagonal       P8,n=n(3n−2)        1, 8, 21, 40, 65, ...
-- The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.
-- The set is cyclic, in that the last two digits of each number is the first two digits of the next number (including the last number with the first).
-- Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal (P5,44=2882), is represented by a different number in the set.
-- This is the only set of 4-digit numbers with this property.
-- Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.

problem0061 :: Int -> Int -> Int -> [[Integer]]
problem0061 digits matches setSize = 
    filter (\s -> isCircular digits s &&
        oneOfEach types s) $ combinations setSize [start..stop]
    where
        start = 10^(toInteger digits - 1)
        stop = 10^(toInteger digits) - 1
        types = [isPoly ((fromIntegral i)+2) | i <- [1..setSize]]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) xs' ++ combinations n xs
    where
        xs' = combinations (n - 1) xs

combinationsTest = [
    [[]] @=? combinations 0 [1..10],
    [] @=? combinations 3 ([]::[Integer]),
    [[n] | n <- [1..10]] @=? combinations 1 [1..10],
    [[1,2]] @=? combinations 2 [1,2],
    [[1,2],[1,3],[2,3]] @=? combinations 2 [1,2,3]
    ]

isCircular :: Int -> [Integer] -> Bool
isCircular = error "Not Implemented"

isCircularTest = [
    True @=? isCircular 1 [12,23,34],
    True @=? isCircular 2 [1234,3456,7890,9012],
    True @=? isCircular 2 [1234,7890,3456,9012],
    True @=? isCircular 2 [7890,3456,1234,9012],
    False @=? isCircular 2 [1111,2222,3333,4444]
    ]

oneOfEach :: [(a -> Bool)] -> [a] -> Bool
oneOfEach = error "Not Implemented"

-- n*(1*n+1)/2 = n(n+1)/2
-- n*(2*n+0)/2 = n*n
-- n*(3*n-1)/2 = n(3n-1)/2
-- n*(4*n-2)/2 = n(2n-1)
-- n*(5*n-3)/2 = n(5n-3)/2
-- pNum = n*((p-2)*n+(4-p))/2
-- pNum = n*(n*p-n*2+4-p)/2
-- 2*pNum = n*(n*p-n*2+(4-p))
-- 2*pNum = n*(n*(p-2)+(4-p))
-- 0 = (p-2)n^2+(4-p)n+(-2*pNum)
-- 0 = n - (-b ٍ± √(b^2 - 4*a*c) / (2*a))
-- 0 = n - ((p-4) ± √((4-p)^2 - 4*(p-2)*(-2*pNum))) / (2*(p-2))
-- n = ((p-4) ± √(16-8p+p^2 + 8*p*pNum-16*pNum)) / (2p-4)
-- n = ((p-4) ± √(p^2+8(pNum-1)p+16(1-pNum))) / (2p-4)
--
-- CASE: pNum == 1
-- n = ((p-4) ± √(p^2+8(1-1)p+16(1-1))) / (2p-4)
-- n = ((p-4) ± √(p^2)) / (2p-4)
-- n = (2p-4)/(2p-4) = 1
-- n = (-4)/(2p-4)
--
-- CASE: pNum == p
-- n = ((p-4) ± √(p^2+8(p-1)p+16(1-p))) / (2p-4)
-- n = ((p-4) ± √(p^2+8p^2-8p+16-16p))) / (2p-4)
-- n = ((p-4) ± √(9p^2-24p+16)) / (2p-4)
-- n = ((p-4) ± √((3p-4)^2)) / (2p-4)
-- n = ((p-4) ± (3p-4)) / (2p-4)
-- n = ((p-4) + (3p-4)) / (2p-4)
-- n = 2*(2p-4) / (2p-4) = 2
-- n = ((p-4) - (3p-4)) / (2p-4)
-- n = (-2p) / (2p-4)
isPoly :: Integer -> Integer -> Bool
isPoly order num = order > 2 && isNthRoot 2 radical && m == 0
    where 
        radical = (order^2) + 8*(num-1)*order + 16*(1-num)
        sqrt_radical = floor $ sqrt $ fromIntegral radical
        m = mod ((order - 4) + sqrt_radical) (2 * order - 4)

isPolyTest = [
    True @=? isPoly 3 1,
    True @=? (all (flip isPoly 1) [3..10]),
    True @=? isPoly 4 4,
    True @=? isPoly 3 3,
    True @=? (all (\n -> isPoly n n) [3..10]),
    False @=? (any (\n -> isPoly n (n-1)) [3..10]),
    True @=? isPoly 3 15,
    False @=? isPoly 3 14,
    True @=? isPoly 4 25,
    True @=? isPoly 5 35,
    True @=? isPoly 6 45,
    True @=? isPoly 7 55,
    True @=? isPoly 8 65
    ]

isNthRoot:: Integer -> Integer -> Bool
isNthRoot n num = nThWorker 0 num
    where
        nThWorker low high
            | low > high = False
            | (mid ^ n) < num = nThWorker (mid + 1) high
            | (mid ^ n) > num = nThWorker low (mid - 1)
            | otherwise = True
            where
                mid = div (high + low) 2

isNthRootTest = [
    False @=? isNthRoot 3 4,
    True @=? isNthRoot 2 4,
    True @=? (all (isNthRoot 2) [1,4,9,16,25]),
    False @=? (any (isNthRoot 2) [2,5,10,17,26]),
    True @=? (all (isNthRoot 3) [1,8,27,64])
    ]

unitTests = map TestCase $
    isPolyTest ++
    isNthRootTest ++
    combinationsTest ++
    isCircularTest

data Arg = Euler | AdHoc { digits::Int, setSize::Int, len::Int } | UnitTest
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = head $ problem0061 4 2 6
        printf "Answer: %d\n" (sum answer)
    exec AdHoc{..} = do
        let answer = problem0061 len digits setSize
        mapM_ (printf "%s\n" . show) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, AdHoc {setSize=3, digits=2, len=4}, UnitTest]
