module Day13
    (
    day13
   ,day13b
   ,_input
    )
    where
import Data.Maybe
import Data.List
import Data.List.Split

day13 :: String -> Int
day13 input = sum $ map (summarize .  rankPattern') 
                  $ parseInput input

day13b :: String -> Int
day13b input = sum $ map (summarize.findAndApplySmudge) 
                   $ parseInput input

summarize :: Num a => (Maybe a,Maybe a) -> a
summarize (h,v) = fromMaybe (fromJust v * 100) h

findAndApplySmudge :: [String] -> (Maybe Int,Maybe Int)
findAndApplySmudge  pat = head $ filter (\z -> z /= (Nothing,Nothing))
        $ map ((\(z,y) -> if z /= current then z else (Nothing,Nothing)) . (\y-> (rankPattern current $ swapAtIndex y pat,y))) [0.. length (concat pat) -1]
       where current = rankPattern' pat

rankPattern' :: Eq a => [[a]] -> (Maybe Int,Maybe Int)
rankPattern' = rankPattern (Nothing,Nothing)

rankPattern :: Eq a => (Maybe Int,Maybe Int) -> [[a]] -> (Maybe Int,Maybe Int)
rankPattern (ignoreVertical,ignoreHorizontal) pat =  (vertical,horizontal)
        where (vertical,horizontal) = (commonSplit ignoreVertical pat, commonSplit ignoreHorizontal $ transpose pat)
              commonSplit i s =  let split = (\(x:xs)-> filter (\y-> Just y/=i &&  all (elem y) xs ) x ) $ map findSplits s
                               in case split of
                                  [] -> Nothing
                                  (y:_) -> Just y

findSplits :: Eq a => [a] -> [Int]
findSplits z = filter (\y-> uncurry (==) $ evenSplit $ splitAt y z) [1..length z-1]
  where evenSplit (a,b) = let minLength = min (length a) (length b)
                          in (take minLength $ reverse a, take minLength b)

swapAtIndex :: Int -> [String] -> [String]
swapAtIndex n ls = chunksOf chunk $ a ++ ((if s =='.' then '#' else '.'):b)
  where (a, s:b) = splitAt n $ concat ls
        chunk = length $ head ls

parseInput :: [Char] -> [[String]]
parseInput input = map lines $ splitOn "\n\n" input

_input="#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"
