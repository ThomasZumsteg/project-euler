module Main where

import Text.Printf

problem0001 :: Int -> [Int] -> Int
problem0001 limit factors = sum (filter (any ) [1..limit])

main = do
    print (problem0001 100 [3, 5])
