module Day2
    (
    day2
   ,day2b
   ,_input
    )
    where
import Data.List.Split
import qualified Data.Map as Map

day2 :: String -> Int
day2 input = sum $ map fst $ filter (\(game,gameMap) -> gameMap Map.! "red" <= 12 && gameMap Map.! "green" <= 13 && gameMap Map.! "blue" <= 14) games
  where games = collapseGames input

day2b :: String -> Int
day2b input = sum $ map (snd . (\(game, m)-> (game, (m Map.! "red") * (m Map.! "green") * (m Map.! "blue")))) (collapseGames input)

parseInput input =  map parseLine $ lines input

collapseGames input = map (`collapseGame` Map.empty) $ parseInput input
collapseGame (game, turns) m = (game,
    foldr (\(k,v) acc -> Map.insertWith max k v acc) m $ concat turns
  )



parseLine input = (game, map parseTurn turns)
  where [gameString, turnsString] = splitOn ": " input
        game = (\y-> read y::Int) $ drop 5 gameString
        turns = splitOn "; " turnsString

parseTurn input = map ((\[x,y]->(y, read x::Int)) . splitOn " ") pulls
  where pulls = splitOn ", " input

_input="Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
