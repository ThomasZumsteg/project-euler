{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf, PrintfArg, formatArg, formatString)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

import Data.List (nub, permutations, intercalate, sort, splitAt)

data Ngon = Ngon { items :: [Integer]} deriving (Show, Eq)

-- [1 2 3 4 5 6]
--    b c a
--  c   b   a
--  b c       a 
-- 4,2,3; 5,3,1; 6,1,2

arms :: Ngon -> [[Integer]]
arms (Ngon{ items=items }) = [[
        items !! (half+i),
        items !! (if i == (half-1) then 0 else (i+1)),
        items !! (if i == 0 then (half-1) else i-1)] | 
    i <- [0..(half-1)]]
    where
        half = div (length items) 2

testArms = [
        [[4,2,3],[5,3,1],[6,1,2]] @=? arms (Ngon { items = [1,2,3,4,5,6] })
    ]

instance PrintfArg Ngon where
    formatArg = formatString . 
        intercalate "; " . map (intercalate "," . map show) . arms

problem0068 :: Integer -> Ngon
problem0068 = error "Not Implemented"

total :: Ngon -> Integer
total = sum . head . arms 

-- inner + outer = (n + 1) * n / 2
-- 7 * 3 = 21
--
-- (2 * inner + outer) / (n / 2)
-- (2 * ((n + 1) * n / 2 - outer) + outer) / (n / 2)
-- 2 * ((n + 1) * n - outer) / n
-- 2 * ((n + 1) * n - outer) / n
--
-- (2 * inner + outer) / (n / 2)
-- (2 * inner + ((n + 1) * n / 2) - inner) / (n / 2)
-- (inner + ((n + 1) * n / 2)) / (n / 2)
-- (2 * inner + ((n + 1) * n)) / n
-- (2 * inner / n) + n + 1 = group sum
--
-- 2 * (1+2+3+4+5) / 10 + 10 + 1 = 3 + 10 + 1 =  14
-- 2 * (6+7+8+9+10) / 10 + 10 + 1 = 8 + 10 + 1 =  19
--
-- 2 * (1+2+3) / 6 + 6 + 1 = 2 + 6 + 1 =  9
-- 2 * (1+2+6) / 6 + 6 + 1 = 3 + 6 + 1 = 10
-- 2 * (2+3+4) / 6 + 6 + 1 = 3 + 6 + 1 = 10
-- 2 * (3+4+5) / 6 + 6 + 1 = 4 + 6 + 1 = 11
-- 2 * (4+5+6) / 6 + 6 + 1 = 5 + 6 + 1 = 12
makeNgonsFaster :: Integer -> [Ngon]
makeNgonsFaster size = [ fromList $ inner ++ outer |
    outer <- outers,
    let inner = makeInner size outer ]
    where
        outers = [ x:xs' |
            (x:xs) <- buildList (div size 2) [1..size],
            0 == mod (2 * sum (x:xs)) size,
            xs' <- permutations xs ]

-- XXX: find a,b,c,d,... given z,y,x,w,... and t
--  i0 i1 i2 o0 o1 o2
-- [1  2  3  4  5  6]
--  o(n) + i(n-1) + i(n-2) = t
--  t - o(0) - i( 2) = i( 1)
--  t - o(1) - i( 1) = i( 0)
--  t - o(2) - i( 0) = i( 2)
--
--  t - o(1) - i( 1) = i( 0)
--  t - o(1) - t + o(0) + i( 2) = i( 0)
--  o(0) - o(1) + i( 2) = i( 0)
--  o(0) - o(1) - o(2) + t  = 2*i( 0)
--  2*4 + 9 - 4 - 5 - 6 = 2 = 2 * 1 = 2*i( 0)
--  2*5 + 9 - 4 - 5 - 6 = 4 = 2 * 2 = 2*i( 1)
--  2*6 + 9 - 4 - 5 - 6 = 6 = 2 * 3 = 2*i( 2)
makeInner :: Integer -> [Integer] -> [Integer]
makeInner size outer = [inner i | i <- [0..((div (fromIntegral size) 2)-1)]]
    where
        s = sum outer
        target = div (2 * ((size + 1) * size - s)) size
        inner i = div (2 * (outer !! i) - s + target) 2


makeInnerTest = [
    [1,2,3] @=? makeInner 6 [4,5,6],
    [1,3,2] @=? makeInner 6 [4,6,5],
    [4,6,5] @=? makeInner 6 [1,3,2]
    ]

makeNgonsFasterTest = [
    (make_ngons 4) @=? (makeNgonsFaster 4)
    ]

buildList :: Integer -> [Integer] -> [[Integer]]
buildList 0 _ = [[]]
buildList _ [] = []
buildList l (x:xs) = (map (x:) (buildList (l-1) xs)) ++ (buildList l xs)

buildListTest = [
    [[1,2,3]] @=? buildList 3 [1..3],
    [[]] @=? buildList 0 [1..3],
    [[1],[2],[3]] @=? buildList 1 [1..3],
    [[1,2],[1,3],[2,3]] @=? buildList 2 [1..3]
    ]

make_ngons :: Integer -> [Ngon]
make_ngons size = filter isMagic $ map fromList $ permutations [1..size]
    where
        isMagic n = (length $ nub $ map sum $ arms n) == 1

fromList :: [Integer] -> Ngon
fromList xs 
    | mod (length xs) 2 /= 0 = error "Not the correct length"
    | sort xs /= [1..(maximum xs)] = error "Not a proper Ngon"
    | otherwise = worker (Ngon { items = xs })
    where
        worker nGon = if correctRotation nGon then nGon else (worker (rotate nGon))

fromListTest = [
    (Ngon{ items = [1,2,3,4,5,6] }) @=? fromList [1,2,3,4,5,6],
    (Ngon{ items = [1,2,3,4,5,6] }) @=? fromList [2,3,1,5,6,4]
    ]

correctRotation :: Ngon -> Bool
correctRotation nGon = minimum leaves == head leaves
    where
        leaves = map head $ arms nGon

correctRotationTest = [
    True @=? (correctRotation $ Ngon { items = [1,2,3,4,5,6] }),
    False @=? (correctRotation $ Ngon { items = [2,3,1,5,6,4] })
    ]

rotate :: Ngon -> Ngon
rotate (Ngon { items = xs }) = fromList (fs ++ [f] ++ ss ++ [s])
    where
        ((f:fs),(s:ss)) = splitAt (div (length xs) 2) xs

unitTests = map TestCase $
    testArms ++
    fromListTest ++
    correctRotationTest ++
    buildListTest ++
    makeNgonsFasterTest ++
    makeInnerTest
    
data Arg = Euler | UnitTest |
    AdHoc { ngon :: Integer } 
    deriving (Show, Data, Typeable)

instance EulerArg Arg where
    exec Euler = do
        let answer = problem0068 5
        printf "Answer %v\n" answer
    exec AdHoc{..} = do
        let answer = make_ngons ngon
        mapM_ (printf "ngon: %v\n") answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { ngon = 5 }]
