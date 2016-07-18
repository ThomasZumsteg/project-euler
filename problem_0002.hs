{-# LANGUAGE DeriveDataTypeable #-}

import Text.Printf
import System.Console.CmdArgs
import Data.Time

data EulerArgs = EulerArgs { limit :: Integer }
    deriving (Show, Data, Typeable)

euler = EulerArgs { limit = 4000000 }

problem0002 :: (Integral a) => a -> a
problem0002 limit = sum $ filter even $ takeWhile (< limit) fiboncci

fiboncci :: [Integer]
fiboncci = 1 : 1 : zipWith (+) fiboncci (tail fiboncci)

main :: IO ()
main = do
    args <- cmdArgs euler
    let EulerArgs{ limit = l } = args
    start <- getCurrentTime
    print $ problem0002 l
    stop <- getCurrentTime
    print $ diffUTCTime stop start
