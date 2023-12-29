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
day13 input = sum $ mapMaybe rankPattern $ parseInput input

day13b :: String -> Int
day13b input = sum $ map findAndApplySmudge $ parseInput input

findAndApplySmudge  pat = head $
        map (\((h,v),_)-> fromMaybe (fromJust v * 100) h)
        $ filter (\(z,_) -> z /= (Nothing,Nothing))
        $ map ((\(z,y) -> if z /= current then (z,y) else ((Nothing,Nothing),y)) . (\y-> (rankPattern'' current $ swapAtIndex y pat,y))) [0.. length (concat pat) -1]
        where current = rankPattern' pat

rankPattern' pat =  (vertical,horizontal)
        where (vertical,horizontal) = (commonSplit pat, commonSplit $ transpose pat)
              commonSplit s =  let split = (\(x:xs)-> filter (\y-> all (elem y) xs ) x ) $ map findSplits s
                               in case split of
                                  [] -> Nothing
                                  (y:_) -> Just y

rankPattern'' (ignoreVertical,ignoreHorizontal) pat =  (vertical,horizontal)
        where (vertical,horizontal) = (commonSplit ignoreVertical pat, commonSplit ignoreHorizontal $ transpose pat)
              commonSplit i s =  let split = (\(x:xs)-> filter (\y-> Just y/=i &&  all (elem y) xs ) x ) $ map findSplits s
                               in case split of
                                  [] -> Nothing
                                  [y] -> Just y
                                  _  -> error "shouldn't be multiple splits"

findSplits z = filter (\y-> uncurry (==) $ evenSplit $ splitAt y z) [1..length z-1]
  where evenSplit (a,b) = let minLength = min (length a) (length b)
                          in (take minLength $ reverse a, take minLength b)

rankPattern :: Eq a => [[a]] -> Maybe Int
rankPattern pat =   case commonSplit' vertical of
                      Nothing -> case commonSplit' horizontal of
                                 Just z -> Just $ 100 * z
                                 Nothing -> Nothing
                      Just x -> Just x
  where (vertical,horizontal) = (pat, transpose pat)
        commonSplit' s = let split = (\(x:xs)-> filter (\y-> all (elem y) xs ) x ) $ map findSplits s
                         in case split of
                            [] -> Nothing
                            [y] -> Just y
                            _  -> error "shouldn't be multiple splits"

swapAtIndex :: Int -> [String] -> [String]
swapAtIndex n ls = chunksOf chunk $ a ++ ((if s =='.' then '#' else '.'):b)
  where (a, s:b) = splitAt n $ concat ls
        chunk = length $ head ls

parseInput :: [Char] -> [[String]]
parseInput input = map lines $ splitOn "\n\n" input

_input="#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"
