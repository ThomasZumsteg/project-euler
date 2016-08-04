{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO( IOMode(ReadMode), openFile, hGetContents)
import Test.HUnit ((@=?), runTestTT, Test(..))
import Data.Maybe (fromJust, isJust)

-- In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
-- The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
-- What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?

data Direction = Horizontal | Vertical | DiagonalUp | DiagonalDown

type Point = (Int, Int)

problem0011 :: (Integral a) => [[a]] -> Int -> [Direction] -> Maybe a
piroblem0011 [[]] _ _ = error "Empty matrix"
problem0011 _ _ [] = Nothing
problem0011 grid l dirs = maximum $ map ((<$>) product) slices
    where
        rows = length grid
        cols = length $ head grid
        points = concat [
            [(r, c) | c <- [0..(pred $ length $ grid !! r)]] 
                | r <- [0..(pred $ length grid)]
            ]
        slices = concat $ map (\d -> map (getSlice grid l d) points) dirs

getSlice :: (Integral a) => [[a]] -> Int -> Direction -> Point -> Maybe [a]
getSlice _ 0 _ _ = Just []
getSlice grid l d p@(r, c) = (:) <$> element <*> elements
    where
        l' = pred l 
        element = getElement grid p
        elements = case d of 
            Vertical -> getSlice grid l' Vertical (succ r, c)
            Horizontal -> getSlice grid l' Horizontal (r, succ c)
            DiagonalUp -> getSlice grid l' DiagonalUp (succ r, pred c)
            DiagonalDown -> getSlice grid l' DiagonalDown (succ r, succ c)

getElement :: (Integral a) => [[a]] -> Point -> Maybe a
getElement grid (r, c)
    | (0 <= r && r < rs) && (0 <= c && c < cs) = Just $ grid !! r !! c
    | otherwise = Nothing
    where
        rs = length grid
        cs = length $ grid !! r

testGridSmall :: [[Integer]]
testGridSmall = [[1,2],[3,4]]

testGridLarge :: [[Integer]]
testGridLarge = [[n..(n+9)] | n <- [0,10..90]]

problem0011Test :: [Test]
problem0011Test = map TestCase [ 
    Just 1 @=? problem0011 [[1]] 1 [Vertical] ,
    Just 1 @=? problem0011 [[1]] 1 [Horizontal] ,
    Just 1 @=? problem0011 [[1]] 1 [DiagonalUp] ,
    Just 1 @=? problem0011 [[1]] 1 [DiagonalDown],
    Just 8 @=? problem0011 testGridSmall 2 [Vertical] ,
    Just 12 @=? problem0011 testGridSmall 2 [Horizontal] ,
    Just 6 @=? problem0011 testGridSmall 2 [DiagonalUp] ,
    Just 4 @=? problem0011 testGridSmall 2 [DiagonalDown],
    Just 12 @=? problem0011 testGridSmall 2 
        [Vertical, Horizontal, DiagonalUp, DiagonalDown]
    ] 

getSliceTest :: [Test]
getSliceTest = map TestCase [
    Just [1]   @=? getSlice testGridSmall 1 Vertical (0,0), 
    Just [1,3] @=? getSlice testGridSmall 2 Vertical (0,0), 
    Just [1,2] @=? getSlice testGridSmall 2 Horizontal (0,0), 
    Just [2,3] @=? getSlice testGridSmall 2 DiagonalUp (0,1),
    Just [1,4] @=? getSlice testGridSmall 2 DiagonalDown (0,0),
    Nothing    @=? getSlice testGridSmall 2 DiagonalUp (1,1),
    Nothing    @=? getSlice testGridSmall 3 DiagonalDown (0,0),
    Just [00,10,20,30] @=? getSlice testGridLarge 4 Vertical (0,0), 
    Just [00,01,02,03] @=? getSlice testGridLarge 4 Horizontal (0,0), 
    Just [03,12,21,30] @=? getSlice testGridLarge 4 DiagonalUp (0,3),
    Just [00,11,22,33] @=? getSlice testGridLarge 4 DiagonalDown (0,0)
    ]

getElementTest :: [Test]
getElementTest = map TestCase [
    Just 1  @=? getElement testGridSmall ( 0, 0),
    Just 4  @=? getElement testGridSmall ( 1, 1),
    Nothing @=? getElement testGridSmall (-1, 0),
    Nothing @=? getElement testGridSmall ( 3, 0),
    Nothing @=? getElement testGridSmall ( 0,-1),
    Nothing @=? getElement testGridSmall ( 0, 3)
    ]

data EulerArgs = 
    Diagonal { sliceLength :: Int, path :: String }
    | UnitTests 
    | Euler 
        deriving (Show, Data, Typeable)
    
diag = Diagonal { sliceLength = 4, path = "problem_0011.txt" }
unittest = UnitTests{}
euler = Euler{}

parseFile :: String -> IO [[Integer]]
parseFile name = do
    file <- openFile name ReadMode
    contents <- hGetContents file
    return $ map (map read . words) $ lines contents

exec :: EulerArgs -> IO ()
exec Diagonal{..} = do
    grid <- parseFile path
    let directions = [DiagonalUp, DiagonalDown]
        result = problem0011 grid sliceLength directions 
    print result
exec Euler = do
    grid <- parseFile "problem_0011.txt"
    let directions = [DiagonalUp, DiagonalDown, Horizontal, Vertical]
        result = problem0011 grid 4 directions
    if isJust result
        then printf "Answer: %d\n" $ fromJust result
        else print "No slices in the grid"
    
exec UnitTests = do 
    runTestTT $ TestList $ getElementTest ++ getSliceTest ++ problem0011Test
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [diag, unittest, euler]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
