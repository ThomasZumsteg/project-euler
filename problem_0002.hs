{-# LANGUAGE DeriveDataTypeable #-}

import Text.Printf
import System.Console.CmdArgs


data EulerArgs = EulerArgs { limit :: Integer }
    deriving (Show, Data, Typeable)

euler = EulerArgs { limit = 4000000 }

problem0002 :: (Integral a) => a -> a
problem0002 limit = limit

fiboncci :: [Integer]
fiboncci = 1 : 1 : zipWith (+) fiboncci (tail fiboncci)

main :: IO ()
main = do
    args <- cmdArgs euler
    let EulerArgs{ limit = l } = args
    print $ sum $ filter even $ takeWhile (< l) fiboncci
