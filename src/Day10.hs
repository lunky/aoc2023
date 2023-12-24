module Day10
    (
    day10
   ,day10b
   ,_input
   ,_input2
    )
    where
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.List.Split
import Data.Bifunctor

translate pipe = case pipe of
  '|' -> Just NorthSouth
  '-' -> Just EastWest
  'L' -> Just NorthEast
  'J' -> Just NorthWest
  '7' -> Just SouthWest
  'F' -> Just SouthEast
  'S' -> Just Start
  _ -> Nothing

xtranslate pipe = case pipe of
   Just NorthSouth -> '|'
   Just EastWest -> '-'
   Just NorthEast -> 'L'
   Just NorthWest ->  'J'
   Just SouthWest -> '7'
   Just SouthEast -> 'F'
   Just Start -> 'S'
   Nothing -> '.'

data Direction = North | East | South | West
                  deriving (Show, Eq)

data Pipe = Start | NorthSouth | EastWest | NorthEast | NorthWest | SouthWest | SouthEast
              deriving (Show, Eq)

day10 :: String -> Int
day10 input = (`div` 2) $ length $ followPath $ parseInput input


followPath :: Map (Int, Int) (Maybe Pipe) -> [(Int, Int)]
followPath m = go (move m (position, direction)) []
  where (position, direction) = getStart m
        go (newPosition,direction') acc
          | Map.lookup newPosition m == Just (Just Start) = newPosition:acc
          | otherwise = go (move m (newPosition,direction')) (newPosition:acc)

followPath' m = go (move m (position, direction)) []
  where (position, direction) = getStart m
        go (newPosition,direction') acc
          | Map.lookup newPosition m == Just (Just Start) = (newPosition,(Map.!) m newPosition):acc
          | otherwise = go (move m (newPosition,direction')) ((newPosition,(Map.!) m newPosition):acc)

-- if the path is a bounding box then the points inside follow a rule - if you 
-- move outward from the center to beyond the extents of the grid and intersect
-- only odd number of edges your point is inside the boundign box.

extents [(x,y)] = [x,y,x,y]
extents (xy:xys) = foldr (\(x',y') [minX, minY, maxX, maxY]
                    ->  [min x' minX, min y' minY, max x' maxX, max y' maxY]) (extents [xy]) xys

day10b' input = filter (\y-> inside' y path e) (empty path)
  where e = extents $ Map.keys parsed
        parsed = parseInput input
        path = replaceStart $ Map.fromList $ followPath' parsed

empty m = map fst $ filter snd $ [((x,y),Map.notMember (x,y) m) | y <- [1..maxY], x <- [1..maxX]]
  where [minX, minY, maxX, maxY] = extents $ map fst $ Map.toList m


inside' (x,y) m [minX, minY, maxX, maxY] =  odd $ score north North
  where
    s = Set.fromList $ Map.keys m
    north = [(\ k -> (k, Map.findWithDefault Nothing k m)) (x, b) | b <- [minY .. y]]

score :: [((Int,Int), Maybe Pipe)] -> Direction -> Int
score xs North = straightPieces + matchingPieces
  where straightPieces = length $ filter (\(_,p)-> p == Just EastWest) xs
        matchingPieces = ((southWest + northEast) `div` 2)  + ((northWest + southEast) `div` 2)
        northEast = length $ filter (\(_,p)-> p == Just NorthEast) xs
        southEast = length $ filter (\(_,p)-> p == Just SouthEast) xs
        southWest = length $ filter (\(_,p)-> p == Just SouthWest) xs
        northWest = length $ filter (\(_,p)-> p == Just NorthWest) xs

getStart :: Map (Int, Int) (Maybe Pipe) -> ((Int, Int), Direction)
getStart m = (startPosition, startDirection startPosition m)
  where startPosition = fst $ head $ filter (\(xy,z)->z==Just Start) $ Map.assocs m

replaceStart :: Map (Int, Int) (Maybe Pipe) -> Map (Int, Int) (Maybe Pipe) 
replaceStart m = Map.insert startPosition (Just $ identifyStart startPosition m) m 
  where startPosition = fst $ head $ filter (\(xy,z)->z==Just Start) $ Map.assocs m

identifyStart p m 
  | n `elem` [Just NorthSouth, Just SouthWest, Just SouthEast] && s `elem` [Just NorthSouth,Just NorthWest, Just NorthEast] = NorthSouth
  | e `elem` [Just EastWest, Just SouthEast, Just NorthEast] && w `elem` [Just EastWest, Just SouthWest, Just NorthWest] = EastWest
  | n `elem` [Just NorthSouth, Just SouthWest, Just SouthEast] && w `elem` [Just EastWest, Just SouthEast, Just NorthEast] = NorthWest
  | s `elem` [Just NorthSouth, Just NorthWest, Just NorthEast] && w `elem` [Just EastWest, Just SouthEast, Just NorthEast] = SouthWest

  | n `elem` [Just NorthSouth, Just SouthWest, Just SouthEast] && e `elem` [Just EastWest, Just SouthWest, Just NorthWest] = NorthEast
  | s `elem` [Just NorthSouth, Just NorthWest, Just NorthEast] && e `elem` [Just EastWest, Just SouthWest, Just NorthWest] = SouthEast
	where  [n,s,e,w] = map (\f-> fromMaybe Nothing $ Map.lookup (f p) m) [north, south, east, west]

startDirection :: (Int, Int) -> Map (Int, Int) (Maybe Pipe) -> Direction
startDirection p m
  | Map.lookup (north p) m `elem` [Just (Just NorthSouth), Just (Just SouthWest), Just (Just SouthEast)] = North
  | Map.lookup (east p) m `elem` [Just (Just EastWest), Just (Just NorthWest), Just (Just SouthWest)] = East
  | Map.lookup (south p) m `elem` [Just (Just NorthSouth), Just (Just NorthWest), Just (Just SouthEast)] = South
  | otherwise = West

move :: Map (Int, Int) (Maybe Pipe) -> ((Int, Int), Direction) -> ((Int, Int), Direction)
move m (p,direction) = (next, turn direction ((Map.!) m next))
    where next =  case direction of
                    North -> north p
                    East -> east p
                    South -> south p
                    West -> west p

north :: (Int, Int) -> (Int, Int)
north (x,y) = (x,y-1)

west :: (Int, Int) -> (Int, Int)
west (x,y)  = (x-1, y)

east :: (Int, Int) -> (Int, Int)
east (x,y)  = (x+1,y)

south :: (Int, Int) -> (Int, Int)
south (x,y) = (x,y+1)

turn :: Direction -> Maybe Pipe -> Direction
turn currentDirection newTurn = case newTurn of
  Just NorthSouth -> currentDirection
  Just EastWest   -> currentDirection
  Just NorthEast  -> if currentDirection == South then East else North
  Just NorthWest  -> if currentDirection == South then West else North
  Just SouthWest  -> if currentDirection == North then West else South
  Just SouthEast  -> if currentDirection == North then East else South

day10b :: String -> Int
day10b input = length $ day10b' input

parseInput :: String -> Map (Int, Int) (Maybe Pipe)
parseInput input = Map.fromList
                    $ concatMap (\(y,xs)->zipWith (\ x z -> ((x, y), translate z)) [1..] xs)
                    $ zip [1..]
                    $ lines input

parseInput2 input = Map.fromList [ ((x,y),'.') | x <- [1..width], y <- [1..height] ]
  where grid = lines input
        width = length $ head grid
        height = length grid

showGrid m = mapM_ putStrLn
              $ chunksOf maxX
              $ [xtranslate
              $ Map.findWithDefault Nothing (x,y) m | y <- [1..maxY], x <- [1..maxX]]
  where [minX, minY, maxX, maxY] = extents $ map fst $ Map.toList m



_input = ".....\n.S-7.\n.|.|.\n.L-J.\n....."
_input2= "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."
_input3="...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n..........."
_input4=".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."
_input5="FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L"
