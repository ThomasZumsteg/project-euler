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
    correctRotationTest
    
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
