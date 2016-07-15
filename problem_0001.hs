{-# LANGUAGE DeriveDataTypeable #-}

-- import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import Text.Printf
import System.Console.CmdArgs

data EulerArgs = EulerArgs { limit :: Integer }
    deriving (Show, Data, Typeable)

euler = EulerArgs { limit = 100 }

problem0001 :: (Integral a) => [a] -> a -> a
problem0001 factors limit = sum (filter divisibleByAny [1..(limit-1)])
    where
        divisibleByAny num = any (\f -> mod num f == 0) factors

main :: IO ()
main = do
    args <- cmdArgs euler
    let EulerArgs{ limit = l } = args
    print (problem0001 [3, 5] l)
