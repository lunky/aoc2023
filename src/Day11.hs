module Day11
    (
    day11
   ,day11b
   ,_input
    )
    where
import Data.List

day11 :: String -> Int
day11 input = sum $ map distance $ uniquePairs $ parseGalaxies $ parseInput 2 input

day11b :: String -> Int -> Int
day11b input multiplier = sum $ map distance $ uniquePairs $ parseGalaxies $ parseInput multiplier input

distance :: ((Int,Int), (Int,Int)) -> Int
distance ((x1,y1), (x2,y2))
   = (max x2 x1 - min x1 x2 ) +  (max y2 y1 - min y1 y2 )

uniquePairs :: Ord b => [b] -> [(b,b)]
uniquePairs l = [(x,y) | x <- l, y <- l, x < y]

parseInput :: Int -> String -> ([String],[Int],[Int])
parseInput z input = (ys,xaxis,yaxis)
  where yaxis = getIndexes z ys
        xaxis = getIndexes z $ transpose ys
        ys = lines input
        getIndexes z ys = tail $ reverse $ foldl' (\acc@(x:xs) y -> 
               if all (=='.') y
               then (x+z):acc
               else (x+1):acc ) [0] ys

parseGalaxies :: ([String], [a],[b]) -> [(a,b)]
parseGalaxies (xs, xaxis,yaxis) =  map fst
                     $ filter (\(_,y)->y=='#')
                     $ concatMap (\(y,xs)->zipWith (\ x z -> ((x, y), z)) xaxis xs)
                     $ zip yaxis xs

_input="...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."
