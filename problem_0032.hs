{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

import Test.HUnit ((@=?), assertBool, runTestTT, Test(..))
import Text.Printf (printf)
import System.Console.CmdArgs
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Maybe (isJust, fromJust, catMaybes)
import Data.List (nub, sort, permutations, find)

data Triplet = Triplet { a::Integer, b::Integer, c::Integer }
    deriving (Show, Eq)

data EulerArgs = 
    AdHoc{start::Int, items::Maybe Int}
    | Euler 
    | UnitTest
    deriving (Show, Data, Typeable)

problem0032 :: [Triplet]
problem0032 = catMaybes $ map (uncurry makeTriplet) $ productAndMultipes "123456789"

makeTriplet :: Integer -> [(Integer, Integer)] -> Maybe Triplet
makeTriplet c [] = Nothing
makeTriplet c ((b,a):abs) 
    | c == a * b = Just (Triplet a b c)
    | otherwise = makeTriplet c abs

productList :: String -> [Triplet]
productList digits = [Triplet (read a') (read b') (read c') |
    perm <- permutations digits,
    m <- [1..(l_digits - 2)],
    n <- [(m + 1)..(l_digits - 1)],
    let (a', b', c') = split2 perm m n]
    where
        l_digits = length digits
        split2 items i j = (a, b, c)
            where
                a = take i items
                b = take (j - i) $ drop i items
                c = drop j items 

productListTest = [
    True @=? elem (Triplet 23 45 16) (productList "123456"),
    (Triplet 1 2 3456789) @=? (head $ drop 0 (productList "123456789"))]

productAndMultipes :: String -> [(Integer, [(Integer, Integer)])]
productAndMultipes digits = [(read c, [(read b, read a) | 
        (a', b') <- filter notNull $ combinations' ab,
        a <- permutations a',
        b <- permutations b']) |
    (c', ab) <- filter size $ combinations digits,
    c <- permutations c',
    let l_ab = length ab - 1]
        where
            notNull (as, bs) = not ((null as) || (null bs))
            size (as, bs) = (not $ null as) && (2 <= length bs)

productAndMultipesTest = [
    (2, [(3, 1)]) @=? (head $ drop 1 $ productAndMultipes "123"),
    (1, [(3, 2)]) @=? (head $ productAndMultipes "123")]

combinations :: [a] -> [([a], [a])]
combinations [] = [([],[])]
combinations (a:as) = map joinFirst as' ++ map joinSecond as'
    where
        as' = combinations as
        joinFirst (bs, cs) = (a:bs, cs)
        joinSecond (bs, cs) = (bs, a:cs)

combinationsTest = [
    True @=? (elem ("123","4") (combinations "1234")),
    True @=? (elem ("14","23") (combinations "1234")),
    True @=? (elem ("3","124") (combinations "1234")),
    True @=? (elem ("4","123") (combinations "1234")),
    False @=? (elem ("4", "13") (combinations "1234")),
    False @=? (elem ("4", "312") (combinations "1234")),
    [("","")] @=? combinations ""]

combinations' :: [a] -> [([a],[a])]
combinations' (i:is) = map joinFirst is'
    where
        is' = combinations is
        joinFirst (bs, cs) = (i:bs, cs)

hasElement element (as, bs) = ((as, bs) == element) || ((bs, as) == element)

combinations'Test = [
    True @=? (any (hasElement ("13579", "2468")) (combinations' "123456789")),
    True @=? (any (hasElement ("1", "234")) (combinations' "1234")),
    True @=? (any (hasElement ("2", "134")) (combinations' "1234")),
    True @=? (any (hasElement ("4", "123")) (combinations' "1234")),
    True @=? (any (hasElement ("12", "34")) (combinations' "1234")),
    [("123",""),("12","3"),("13","2"),("1","23")] @=? combinations' "123"]

unitTests = map TestCase $
    productListTest ++
    combinationsTest ++ 
    productAndMultipesTest ++
    combinations'Test

exec :: EulerArgs -> IO ()
exec AdHoc{..} = do
    let answer = if isJust items
        then take (fromJust items) $ drop start problem0032
        else drop start problem0032
    printf "List of Pandigital products:\n" 
    mapM_ (\(Triplet a b c) -> printf "%d * %d = %d\n" a b c)  answer
exec Euler = do
    let answer = sum $ nub $ map c problem0032 
    printf "Answer: %s\n" (show answer)
exec UnitTest = do 
    runTestTT $ TestList unitTests
    return ()

main :: IO ()
main = do
    args <- cmdArgs $ modes [
        AdHoc{start = 0, items = Nothing},
        Euler,
        UnitTest]
    start <- getCurrentTime
    exec args
    stop <- getCurrentTime
    print $ diffUTCTime stop start
