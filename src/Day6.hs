module Day6
    (
    day6
   ,day6b
   ,_input
    )
    where

import Data.Char

day6 :: String -> Int
day6 input =  product $ map day6' $ parseInput input

day6' (time,distance) = length $ filter (>distance) $ generate time

day6b :: String -> Int
day6b input =  product $ map day6' $ parseInput' input


generate :: Int -> [Int]
generate time = map (\y -> y * (time - y) )  [0..time]
  
parseInput :: String -> [(Int,Int)]
parseInput input = zip times distance
  where [times,distance] = map (map (\y -> read y::Int) . tail . words) $ lines input

parseInput' :: String -> [(Int,Int)]
parseInput' input =  (\[a,b]->[(a,b)]) $ map (\y->read $ filter (\z->isNumber z) y) $ lines input

_input="Time:      7  15   30\nDistance:  9  40  200"

