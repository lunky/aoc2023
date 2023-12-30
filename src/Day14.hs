{-# LANGUAGE NumericUnderscores #-}
module Day14
    (
    day14
   ,day14b
   ,_input
    )
    where
import qualified Data.Map as Map
import Data.List
import Data.List.Split

-- Todo refactor this, there's so much code duplication...

data Direction = North | South | East | West deriving (Show,Eq)

day14 :: String -> Int
day14 input = scoreGrid $ tiltNorth e m
  where e = extents $ Map.keys m
        m = parseInput input

day14b :: String -> Int
day14b input = scoreGrid $ (a ++ b) !! ((1_000_000_000 - length a) `mod` length b + length a)
  where (a,b) = findCycle $ iterate doCycle $ parseInput input

findCycle :: Eq a => [a] -> ([a],[a])
findCycle xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x == y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys

doCycle' (m,x) = let result = doCycle m
                 in (result, scoreGrid result)

doCycle m = tiltEast e $ tiltSouth e $ tiltWest e $ tiltNorth e m
  where e = extents $ Map.keys m

scoreGrid m = Map.foldrWithKey (\(_,y) v acc-> (maxY+1-y) + acc  ) 0 $ Map.filter (=='O') m
  where [_, _, _, maxY] = extents $ Map.keys m

tiltNorth e m = foldl' (\acc y-> if (Map.!) acc y == 'O' then moveRockNorth e acc y else acc) m
             [(x,y) | y<-[minY..maxY], x <- [minX..maxX]]
   where [minX, minY, maxX, maxY] = e

tiltSouth e m = foldl' (\acc y-> if (Map.!) acc y == 'O' then moveRockSouth e acc y else acc) m
             [(x,y) | y<-[maxY,maxY-1..minY], x <- [minX..maxX]]
   where [minX, minY, maxX, maxY] = e

tiltEast e m = foldl' (\acc y-> if (Map.!) acc y == 'O' then moveRockEast e acc y else acc) m
             [(x,y) | x <- [maxX,maxX-1..minX],y<-[minY..maxY]]
   where [minX, minY, maxX, maxY] = e

tiltWest e m = foldl' (\acc y-> if (Map.!) acc y == 'O' then moveRockWest e acc y else acc) m
             [(x,y) | x <- [minX..maxX],y<-[minY..maxY]]
   where [minX, minY, maxX, maxY] = e

moveRockNorth e m (x,y) =  let northest = goNorth minY (x,y) m
                     in  if snd northest /= y
                         then Map.insert (x,y) '.' $ Map.insert northest 'O' m
                         else m
  where [_, minY, _, _] = e
moveRockSouth e m (x,y) =  let southest = goSouth maxY (x,y) m
                     in  if snd southest /= y
                         then Map.insert (x,y) '.' $ Map.insert southest 'O' m
                         else m
  where [_, _, _, maxY] = e

moveRockEast e m (x,y) =  let eastest = goEast maxX (x,y) m
                     in  if fst eastest /= x
                         then Map.insert (x,y) '.' $ Map.insert eastest 'O' m
                         else m
  where [_, _, maxX, _] = e

moveRockWest e m (x,y) =  let westest = goWest minX (x,y) m
                     in  if fst westest /= x
                         then Map.insert (x,y) '.' $ Map.insert westest 'O' m
                         else m
  where [minX, _, _, _] = e

goNorth minY (x,y) m =   let curr = Map.lookup (north (x,y)) m
                     in case curr of
                         Just v ->  if v /= '.'  ||  y == minY
                                    then (x,y)
                                    else goNorth minY (north (x,y)) m
                         Nothing -> (x,y)

goSouth maxY (x,y) m =   let curr = Map.lookup (south (x,y)) m
                     in case curr of
                         Just v ->  if v /= '.'  ||  y == maxY
                                    then (x,y)
                                    else goSouth maxY (south (x,y)) m
                         Nothing -> (x,y)

goEast maxX (x,y) m =   let curr = Map.lookup (east (x,y)) m
                     in case curr of
                         Just v ->  if v /= '.'  ||  x == maxX
                                    then (x,y)
                                    else goEast maxX (east (x,y)) m
                         Nothing -> (x,y)

goWest minX (x,y) m =   let curr = Map.lookup (west (x,y)) m
                     in case curr of
                         Just v ->  if v /= '.'  ||  x == minX
                                    then (x,y)
                                    else goWest minX (west (x,y)) m
                         Nothing -> (x,y)

north (x,y) = (x,y-1)
south (x,y) = (x,y+1)
east (x,y) = (x+1,y)
west (x,y) = (x-1,y)

parseInput input = Map.fromList
                    $ concatMap (\(y,xs)->zipWith (\ x z -> ((x, y), translate z)) [1..] xs)
                    $ zip [1..]
                    $ lines input


translate c = c
xtranslate c = c

_showGrid m = mapM_ putStrLn
              $ chunksOf maxX
              $ [xtranslate
              $ Map.findWithDefault '.' (x,y) m | y <- [1..maxY], x <- [1..maxX]]
  where [_, _, maxX, maxY] = extents $ map fst $ Map.toList m

extents [(x,y)] = [x,y,x,y]
extents (xy:xys) = foldr (\(x',y') [minX, minY, maxX, maxY]
                    ->  [min x' minX, min y' minY, max x' maxX, max y' maxY]) (extents [xy]) xys

_input="O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
