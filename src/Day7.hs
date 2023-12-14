module Day7
    (
    day7
   ,day7b
   ,_input
    ) where
import Data.Char
import Data.List
import Data.Bifunctor

day7 :: String -> Int
day7 input = sum
               $ zipWith (*) [1..]
               $ map snd
               $ sortBy compare'
               $ map (first rank)
               $ parseInput input

day7b :: String -> Int
day7b input = sum
               $ zipWith (*) [1..]
               $ map snd
               $ sortBy compare''
               $ map (first rank')
               $ parseInput input


compare' (a,_) (b,_) = if sortOrder a == sortOrder b
              then map card (hand a) `compare` map card (hand b)
              else
                compare (sortOrder a) (sortOrder b)

compare'' (a,_) (b,_) = if sortOrder a == sortOrder b
              then map card' (hand a) `compare` map card' (hand b)
              else
                compare (sortOrder a) (sortOrder b)

data Card = UnderTen { cardNum :: Int } | T | J | Q | K | A deriving (Ord, Eq, Show)
data Card' = J' | UnderTen' Int | T' | Q' | K' | A' deriving (Ord, Eq, Show)

card :: Char -> Card
card a = case a of
           'T' -> T
           'J' -> J
           'Q' -> Q
           'K' -> K
           'A' -> A
           a -> UnderTen $ digitToInt a

card' :: Char -> Char
card' a = case a of
           'A' -> 'D'
           'K' -> 'C'
           'Q' -> 'B'
           'T' -> 'A'
           'J' -> '0'
           a -> a

data Rank = High { hand :: String }
          | Pair { hand :: String }
          | TwoPair { hand :: String }
          | Three { hand :: String }
          | Four { hand :: String }
          | Five { hand :: String }
          | FullHouse { hand :: String } deriving (Show, Eq)

sortOrder :: Rank -> Int
sortOrder a = case a of
              High _ -> 1
              Pair _ -> 2
              TwoPair  _ -> 3
              Three _ -> 4
              FullHouse _ -> 5
              Four _ -> 6
              Five _ -> 7

parseInput :: String -> [(String,Int)]
parseInput input = map ((\[a,b]->(a, read b)) . words) $ lines input


rank hand
  | 5 `elem` pat = Five hand
  | 4 `elem` pat = Four hand
  | 3 `elem` pat && 2 `elem` pat = FullHouse hand
  | 3 `elem` pat = Three hand
  | (>1) $ length $ filter (==2) pat = TwoPair hand
  | (==1) $ length $ filter (==2) pat = Pair hand
  | otherwise = High hand
  where pat = (sort.map length.group.sort) hand

rank' hand
  | 5 `elem` pat = Five hand
  | wild == 4 = Five hand
  | wild == 3 && onePair = Five hand
  | wild == 2 && threeOfAKind = Five hand
  | wild == 1 && fourOfAKind = Five hand
  | wild == 3 = Four hand
  | wild == 2 && onePair = Four hand
  | wild == 1 && threeOfAKind = Four hand
  | fourOfAKind = Four hand
  | wild == 1 && twoPairs = FullHouse hand
  | threeOfAKind && onePair = FullHouse hand
  | threeOfAKind = Three hand
  | wild == 1 && onePair = Three hand
  | wild == 2 = Three hand
  | twoPairs = TwoPair hand
  | wild == 1 = Pair hand
  | onePair = Pair hand
  | otherwise = High hand
  where pat = (sort.map length.group.sort) hand
        notWild = (sort.map length.group.sort.filter (/='J')) hand
        wild = length $ filter (=='J') hand
        onePair = 2 `elem` notWild
        threeOfAKind = 3 `elem` notWild
        fourOfAKind = 4 `elem` notWild
        twoPairs = (==2) $ length $ filter (==2) notWild

_input="32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
