{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified Data.Set as Set
import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.List (sort, tails)

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

problem0061 :: Int -> Int -> [[Integer]]
problem0061 digits setSize = do
    a <- upperLimit $ lowerLimit $ polyGen 3
    let tri = a
    b <- notIn [tri] $ upperLimit $ lowerLimit $ polyGen 4
    let squ = b
    c <- notIn [tri,squ] $ upperLimit $ lowerLimit  $ polyGen 5
    let pen = c
    d <- notIn [tri,squ,pen] $ upperLimit $ lowerLimit  $ polyGen 6
    let hex = d
    e <- notIn [tri,squ,pen,hex] $ upperLimit $ lowerLimit  $ polyGen 7
    let hep = e
    f <- notIn [tri,squ,pen,hex,hep] $ upperLimit $ lowerLimit  $ polyGen 8
    let oct = f
    filter (cyclical 2) [[tri,squ,pen,hex,hep,oct]]
    where
        upperLimit = takeWhile (<(10^digits-1))
        lowerLimit = dropWhile (<(10^(digits-1)))
        notIn others = filter (not . flip elem others)


cyclical :: (Show a) => Int -> [a] -> Bool
cyclical chars xs@(x:xs') = all match $ zip xs (xs' ++ [x])
    where
        match (y, z) = (take chars $ show z) == 
            (reverse $ take chars $ reverse $ show y)

cyclicalTest = [
    True @=? cyclical 1 [1,1,1],
    True @=? cyclical 1 [12,21],
    True @=? cyclical 1 [12,23,31],
    True @=? cyclical 1 [123,345,561],
    True @=? cyclical 2 [12345,45678,78912],
    False @=? cyclical 1 [12,23,34]
    ]

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

polyGen :: Integer -> [Integer]
polyGen n = scanl (+) 1 [(n-1), (n-2) + (n-1)..]

triangle   = polyGen 3 -- [2,3,4,5] 1
square     = polyGen 4 -- [3,5,7,9] 2 
pentagonal = polyGen 5 -- [4,7,10,13] 3
hexagonal  = polyGen 6 -- [5,9,13,17] 4
heptagonal = polyGen 7
octagonal  = polyGen 8

testPolyGen = [
    [1,3,6,10] @=? (take 4 $ polyGen 3),
    [1,4,9,16] @=? (take 4 $ polyGen 4),
    [1,5,12,22] @=? (take 4 $ polyGen 5)
    ]

unitTests = map TestCase $
    testPolyGen ++
    cyclicalTest ++
    isPolyTest

data Arg = Euler | AdHoc { digits::Int, setSize::Int } | UnitTest
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = head $ problem0061 4 6
        printf "Answer: %d\n" (sum answer)
    exec AdHoc{..} = do
        let answer = problem0061 digits setSize
        mapM_ (printf "%s\n" . show) answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, AdHoc {setSize=4, digits=4}, UnitTest]
