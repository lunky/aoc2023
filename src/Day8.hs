module Day8
    (
    day8
   ,day8b
   ,_input
   ,_input2
   ,_input3
    )
    where
import Data.Map (Map)
import qualified Data.Map as Map

chooser :: String -> Char -> Map String (String,String) -> String
chooser key direction m = case direction of
                            'L' -> fst value
                            'R' -> snd value
    where
      value = (Map.!) m key

parseInput :: String -> (String, Map String (String,String))
parseInput input = (instructions, elementMap)
  where (instructions:(_:elements)) = lines input
        elementMap = Map.fromList $ map (\y-> (take 3 y,(take 3 $ drop 7 y, take 3 $ drop 12 y))) elements

day8 :: String -> Int
day8 input = day8'' 0 "AAA" (cycle instructions)
  where
    (instructions, elements) = parseInput input
    day8'' current newKey instr
      | newKey == "ZZZ" = current
      | otherwise = day8'' (current+1) (chooser newKey (head instr) elements) (tail instr)


-- ughhh just do it once for each match and get the lowest common multiple of all
day8b :: String -> Integer
day8b input = foldr1 lcm $ map (\k -> day8'' 0 k (cycle instructions)) keys
  where 
    (instructions, elements) = parseInput input
    keys = filter (\[_,_,y] -> y=='A') $ Map.keys elements
    day8'' current newKey instr
      | last newKey =='Z' = current
      | otherwise = day8'' (current+1) (chooser newKey (head instr) elements) (tail instr)

_input="RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"
_input2="LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
_input3="LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"
