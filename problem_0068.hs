{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Test.HUnit ((@=?), runTestTT, Test(..))
import Text.Printf (printf, PrintfArg, formatArg, formatString)
import System.Console.CmdArgs

import Common (exec, EulerArg, euler_main)

import Data.List (nub, permutations, intercalate, sort, splitAt)

data Ngon = Ngon { items :: [Integer]} deriving (Show, Eq, Ord)

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
problem0068 n = last $ makeNgonsFaster n

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
makeNgonsFaster size = [ Ngon { items = inner ++ outer } |
    outer <- outers,
    let inner = makeInner size outer
        items = inner ++ outer,
        sort items == [1..size]]
    where
        outers = [ x:xs' |
            (x:xs) <- buildList (div size 2) [1..size],
            0 == mod (2 * sum (x:xs)) size,
            xs' <- permutations xs ]

-- XXX: find a,b,c,d,... given z,y,x,w,... and t
--  i0 i1 i2 o0 o1 o2
-- [1  2  3  4  5  6] target = 9
-- i[0] = t - o[1] + o[0] - o[2]
--  2*1 = 9 -   5  +   4  -   6
--
--  t - o(1) - i(2) = i(0) # 9 - 5 - 3 = 1
--  t - o(2) - i(0) = i(1) # 9 - 6 - 1 = 2
--  t - o(0) - i(1) = i(2) # 9 - 4 - 2 = 3
--  i(0) = t - o(1) - i(2)
--  i(0) = t - o(1) - (t - o(0) - i(1))
--  i(0) = - o(1) + o(0) + i(1)
--  i(0) = - o(1) + o(0) + (t - o(2) - i(0))
--  i(0) = t - o(1) + o(0) - o(2) - i(0)
--  2*i(0) = t + o(0) - o(1) - o(2)
--
--  i(1) = t - o(2) - i(0)
--  i(1) = t - o(2) - (t - o(1) - i(2))
--  i(1) = - o(2) + o(1) + i(2)
--  i(1) = - o(2) + o(1) + (t - o(0) - i(1))
--  i(1) = t - o(2) + o(1) - o(0) - i(1)
--  2*i(1) = t + o(1) - o(2) - o(0)
--
--  i0 i1 i2 i3 i4 o0 o1 o2 o3 o4
-- [4, 3, 2, 1, 5, 6, 8, 10,7, 9 ]
-- 6,3,5; 8,2,4; 10,1,3; 7,5,2; 9,4,1
-- target = 14
--  t - o(1) - i(2) = i(0) # 14 - 8 - 2 = 4
--  t - o(2) - i(3) = i(1) # 14 -10 - 1 = 3
--  t - o(3) - i(4) = i(2) # 14 - 7 - 5 = 2
--  t - o(4) - i(0) = i(3) # 14 - 9 - 4 = 1
--  t - o(0) - i(1) = i(4) # 14 - 6 - 3 = 5
--
-- i(0) = t - o(1) - i(2)
-- i(0) = t - o(1) - (t - o(3) - i(4))
-- i(0) = - o(1) + o(3) + i(4)
-- i(0) = - o(1) + o(3) + (t - o(0) - i(1))
-- i(0) = t - o(1) + o(3) - o(0) - i(1)
-- i(0) = t - o(1) + o(3) - o(0) - (t - o(2) - i(3))
-- i(0) = - o(1) + o(3) - o(0) + o(2) + i(3)
-- i(0) = - o(1) + o(3) - o(0) + o(2) + (t - o(4) - i(0))
-- i(0) = t - o(1) + o(3) - o(0) + o(2) - o(4) - i(0)
-- 2*i(0) = t - o(0) - o(1) + o(2) + o(3) - o(4)
-- 2*i(1) = t - o(0) - o(1) - o(2) + o(3) + o(4) # 14 - 6 - 8 -10 + 7 + 9 = 6 = 2*3
--
--  i0 i1 i2 i3 i4 i5 i6 o0 o1 o2 o3 o4 o5 o6
-- [ 2, 6, 7, 4, 1, 3, 5, 8,10, 9,11,12,13,14] target = 19
-- i[0] =  t - o[1] - o[2] + o[3] + o[4] - o[5] - o[6]
-- i[0] =  t + o[0] - o[1] - o[2] + o[3] + o[4] - o[5] - o[6]
--   2  = 19 +   8  -  10  -   9  +  11  +  12  -  13  -  14
--    2:-       (take 1 $ drop 2) 0
--    6:+--     (take 3 $ drop 1) 1
--   10:--++-   (take 5 $ drop 2) 2
--   14:+--++-- (take 7 $ drop 1) 3
-- 1,3,7; 2,7,2; 6,2,3 = 11 ok
-- 1,4,5; 5,5,0; 6,0,4 = 10 xx
-- 1,4,6; 3,6,2; 5,2,4 = 11 ok
-- 1,5,4; 6,4,0; 5,0,5 = 10 xx
-- 1,6,4; 5,4,2; 3,2,6 = 11 ok
-- 1,7,3; 6,3,2; 2,2,7 =  9 xx
-- 2,3,5; 4,5,1; 6,1,3 = 10 ok
-- 2,5,3; 6,3,1; 4,1,5 = 10 ok
-- 3,3,4; 4,4,2; 5,2,3 = 10 xx
-- 3,4,3; 5,3,2; 4,2,4 = 10 xx
-- 4,2,3; 5,3,1; 6,1,2 =  9 ok
-- 4,3,2; 6,2,1; 5,1,3 =  9 ok
--
-- 4,2,3; 5,3,1; 6,1,2 =  9
-- 4,3,2; 6,2,1; 5,1,3 =  9
-- 2,3,5; 4,5,1; 6,1,3 = 10
-- 2,5,3; 6,3,1; 4,1,5 = 10
-- 1,4,6; 3,6,2; 5,2,4 = 11
-- 1,6,4; 5,4,2; 3,2,6 = 11
-- 1,5,6; 2,6,4; 3,4,5 = 12
-- 1,6,5; 3,5,4; 2,4,6 = 12
makeInner :: Integer -> [Integer] -> [Integer]
makeInner size outer = [div (target + inner i) 2 |
        i <- [0..((div (fromIntegral size) 2)-1)]]
    where
        s = sum outer
        target = div (2 * ((size + 1) * size - s)) size
        masks = map (\(d,t) -> take t $ drop d $ cycle [1,1,-1,-1]) $ 
                zip (cycle [2,1]) [1,3..] 
        mask = masks !! (fromIntegral $ div (size-2) 4)
        inner i = sum $ map (uncurry (*)) $ zip (reverse outer) $ drop (i+1) $ cycle mask

makeInnerTest = [
    [1,2,3] @=? makeInner 6 [4,5,6],
    [1,3,2] @=? makeInner 6 [4,6,5],
    [4,6,5] @=? makeInner 6 [1,3,2],
    [4,3,2,1,5] @=? makeInner 10 [6,8,10,7,9]
    ]

makeNgonsFasterTest = [
    (sort $ nub $ make_ngons 6) @=? (sort $ makeNgonsFaster 6)
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
    | mod (length xs - 2) 4 /= 0 = error "Not the correct length"
    | sort xs /= [1..(maximum xs)] = error ("Not a proper Ngon { items= " ++ show xs ++ " }")
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
        let answer = problem0068 10
        printf "Answer %v\n" (concat $ concat $ map (map show) $ arms answer)
    exec AdHoc{..} = do
        let answer = makeNgonsFaster ngon
        mapM_ (printf "ngon: %v\n") answer
    exec UnitTest = do
        runTestTT $ TestList unitTests
        return ()

main :: IO ()
main = euler_main [Euler, UnitTest, AdHoc { ngon = 6 }]
