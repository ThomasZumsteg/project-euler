module Main where

import Text.Printf

problem0001 :: Int -> [Int] -> Int
problem0001 limit factors = sum ( filter ( divisibleByAny factors ) [1..( limit - 1)])

divisibleByAny :: [Int] -> Int -> Bool
divisibleByAny factors num = any (\f -> mod num f == 0) factors

main = do
    print (problem0001 1000 [3, 5])
