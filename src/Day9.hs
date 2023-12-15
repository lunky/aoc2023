module Day9
    (
    day9
   ,day9b
   ,_input
   ,day9'
    )
    where

day9 :: String -> Int
day9 input = sum $ map day9' $ parseInput input

day9' inSet = sum $ go inSet [last inSet]
 where
  go (x:xs) acc
   | all (==0) (x:xs) = acc
   | otherwise = go next (last next:acc)
    where next = zipWith (flip (-)) (x:xs) xs

day9b :: String -> Int
day9b input = sum $ map day9b' $ parseInput  input

day9b' inSet = foldl1 (\acc z-> z-acc) $ go inSet [head inSet]
 where
  go (x:xs) acc
   | all (==0) (x:xs) = acc
   | otherwise = go next (head next:acc)
    where next = zipWith (flip (-)) (x:xs) xs

parseInput :: String -> [[Int]]
parseInput input = map (map read.words) $ lines input


_input="0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
