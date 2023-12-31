module Day4
    (
    day4
   ,day4b
   ,_input
    )
    where

import qualified Data.Map as Map
import Data.List

day4 :: String -> Int
day4 input = sum $ map (score . snd) (parseInput input)

day4b :: String -> Int
day4b input = sum $ map snd $ Map.toList $ day4b' input
  where 
    day4b' input = foldl' award initialized boardState
      where boardState = parseInput input
            initialized = Map.fromList $ map (\y-> (fst y,1)) boardState
            award acc (game,wins) = foldr (\y m ->  -- add cards for the next `wins` number of games
                                     Map.insertWith (+) y (m Map.! game) m ) 
                                     acc [(game+1)..(game+wins)]

score :: Int -> Int
score 0 = 0
score n = 2 ^ (n-1)

parseInput :: String -> [(Int, Int)]
parseInput input = map parseCard $ lines input

parseCard :: [Char] -> (Int, Int)
parseCard input = (card, matches (winners,drawn))
  where card = (\y->read y::Int) $ takeWhile (/=':') $ drop 5 input
        winners = words $ takeWhile (/='|') $ drop 2 $ dropWhile (/=':') input
        drawn = words $ drop 2 $ dropWhile (/='|') input
        matches (w,d) = length $ filter (==True) $ map (`elem` d) w

_input="Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
