module Day16
    (
    day16
   ,day16b
   ,_input
    )
    where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Control.Parallel.Strategies (parMap, rdeepseq)

data Direction = North | East | South | West deriving (Show, Eq, Ord)
data Point = Point Int Int deriving (Show, Eq, Ord)
data Location = Location Point Direction deriving (Show, Eq, Ord)


day16 :: String -> Int
day16 input = measure $ day16' start $ parseInput input

point (Location p _) = p

day16b :: String -> Int
day16b input = maximum $ day16b' input 
  where 
    day16b' input = parMap rdeepseq (\y-> measure $ day16' y m) $ possibleStarts $ extents m 
    m = parseInput input

measure ls = length $ nub $ map point $ Set.toList $ snd ls

day16' itsStart m = head $ dropWhile movesRemaining $ iterate move' ([itsStart],Set.empty)
  where
        movesRemaining (xs,_) = not (null xs)
        move' :: ([Location],Set.Set Location) -> ([Location],Set.Set Location)
        move'  (x:xs,seen) =  if Set.member x seen
                              then (xs,seen)
                              else
                                case mapLookup x m of
                                  Nothing -> (xs,seen)
                                  Just c -> (go x c ++ xs,Set.insert x seen)
          where mapLookup (Location (Point x y) dir) = Map.lookup (x,y)


go :: Location -> Char -> [Location]
go (Location (Point x y) North) '.' = [Location (Point x (y-1)) North]
go (Location (Point x y) South) '.' = [Location (Point x (y+1)) South]
go (Location (Point x y) East) '.' = [Location (Point (x+1) y) East]
go (Location (Point x y) West) '.' = [Location (Point (x-1) y) West]
go (Location (Point x y) North) '|' = [Location (Point x (y-1)) North]
go (Location (Point x y) South) '|' = [Location (Point x (y+1)) South]
go (Location (Point x y) East) '|' = [Location (Point x (y-1)) North,Location (Point x (y+1)) South]
go (Location (Point x y) West) '|' = [Location (Point x (y-1)) North,Location (Point x (y+1)) South]
go (Location (Point x y) North) '-' = [Location (Point (x+1) y) East,Location (Point (x-1) y) West]
go (Location (Point x y) South) '-' = [Location (Point (x+1) y) East,Location (Point (x-1) y) West]
go (Location (Point x y) East) '-' = [Location (Point (x+1) y) East]
go (Location (Point x y) West) '-' = [Location (Point (x-1) y) West]
go (Location (Point x y) North) '/' = [Location (Point (x+1) y) East]
go (Location (Point x y) South) '/' = [Location (Point (x-1) y) West]
go (Location (Point x y) East) '/' = [Location (Point x (y-1)) North]
go (Location (Point x y) West) '/' = [Location (Point x (y+1)) South]
go (Location (Point x y) North) '\\' = [Location (Point (x-1) y) West]
go (Location (Point x y) South) '\\' = [Location (Point (x+1) y) East]
go (Location (Point x y) East) '\\' = [Location (Point x (y+1)) South]
go (Location (Point x y) West) '\\' = [Location (Point x (y-1)) North]

start :: Location
start = Location (Point 1 1) East

parseInput :: String -> Map.Map (Int,Int) Char
parseInput input = Map.fromList
                    $ concatMap (\(y,xs)->zipWith (\ x z -> ((x, y), translate z)) [1..] xs)
                    $ zip [1..]
                    $ lines input
        where translate c = c

_showGrid m = mapM_ putStrLn
              $ chunksOf maxX
              $ [xtranslate
              $ Map.findWithDefault '.' (x,y) m | y <- [1..maxY], x <- [1..maxX]]
  where [_, _, maxX, maxY] = extents m
        xtranslate c = c

extents m = extents' $ map fst $ Map.toList m
  where extents' [(x,y)] = [x,y,x,y]
        extents' (xy:xys) = foldr (\(x',y') [minX, minY, maxX, maxY]
                    ->  [min x' minX, min y' minY, max x' maxX, max y' maxY]) (extents' [xy]) xys
_input=".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|...."
_input2=".....|....\n..........\n....|--...\n..........\n.........."


possibleStarts [_,_,maxX,maxY] = top ++ bottom ++ left ++ right
    where
        top = [Location (Point x 1) South | x <- [1..maxX]]
        bottom = [Location (Point x maxY) North | x <- [1..maxX]]
        left = [Location (Point 1 y) East | y <- [1..maxY]]
        right = [Location (Point maxX y) West | y <- [1..maxY]]

