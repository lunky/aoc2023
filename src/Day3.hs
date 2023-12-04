module Day3
    (
    day3
   ,day3b
   ,_input
    )
    where

import Data.List.Split
import qualified Data.Map as Map
import Data.Char

day3 :: String -> Int
day3 input = sum
                $ map ((\y-> read y::Int) . map snd)
                  (filter (any (\k-> Map.member k m && isSymbol' ( m Map.! k)) . concatMap (\(k,v)->adjacent k))
                  $ findNumbers input)
   where m = parseInput input

findNumbers input = filter (not . null)
                      $ concatMap ( splitWhen (\(k,v)-> not $ isNumber v))
                      $ parseInput' input

isSymbol' c = not (isLetter c) && not (isNumber c) && (c/='.')



day3b :: String -> Int
day3b input = sum $ map product
    $ map (map (\y-> (\z->read z::Int) $ (map snd) y))
    $ filter (\y -> length y == 2)
    $ map (\p-> filter (any (\z->z==fst p) . concat . adjacentToNumber)  numbers ) gears
  where gears = findGears input
        numbers = findNumbers input

findGears input = filter (\(k,v)-> v=='*') $ concat $ parseInput' input


data Point = Point Int Int deriving (Show)

instance Ord Point
  where compare (Point x y) (Point x2 y2) = compare (x,y) (x2,y2)

instance Eq Point
  where (Point x1 y1) == (Point x2 y2) = (x1,y1) == (x2,y2)

adjacentToNumber = map (\(k,v)-> adjacent k)

adjacent (Point x y) = [
                      Point (x - 1) (y - 1), Point x (y - 1), Point (x + 1) (y -1),
                      Point (x - 1)  y,                       Point (x + 1) y,
                      Point (x - 1) (y + 1), Point x (y + 1), Point (x + 1) (y + 1)
                       ]


parseInput :: [Char] -> Map.Map Point Char
parseInput input = Map.fromList (concat parsed)
    where parsed = zipWith (\y row -> map (\(x, c) -> (Point x y, c)) row) [0..]
                                                    (map (zip [0..]) $ lines input)

parseInput' input = zipWith (\y row -> map (\(x, c) -> (Point x y, c)) row) [0..]
                                                    (map (zip [0..]) $ lines input)


_input="467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
