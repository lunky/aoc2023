{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Day12
    (
    day12
   ,day12b
   ,_input
   ,unfold
    )
    where
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Control.Monad.State

type MT = M.Map (String, [Int]) Int

day12 :: String -> Int
day12 input =  sum . map (uncurry ways) $ parseInput input

day12b :: String -> Int
day12b input = sum . map (uncurry ways) $ unfold $ parseInput input

parseInput :: String -> [(String,[Int])]
parseInput input = map parseLine $ lines input
  where parseLine input = (takeWhile (/=' ') input, map (\y->read y::Int) $ splitOn "," $ tail $ dropWhile (/=' ') input)

unfold :: [(String, [Int])] -> [(String, [Int])]
unfold = map (\(s, xs) -> (intercalate "?" (replicate 5 s), concat (replicate 5 xs)))

ways :: String -> [Int] -> Int
ways s = flip evalState M.empty . memo ways' s
  where
    memo mf s xs = let key = (s, xs) in gets (M.lookup key) >>= \case
      Just v -> pure v
      Nothing -> mf (memo mf) s xs >>= \v -> modify (M.insert key v) >> pure v
    ways' :: (String -> [Int] -> State MT Int) -> String -> [Int] -> State MT Int
    ways' f [] [] = pure 1
    ways' f [] [x] = pure 0
    ways' f s [] = pure (if (notElem '#' . nub) s then 1 else 0)
    ways' f ('.':rs) xs = f rs xs
    ways' f ('?':rs) xs = f rs xs >>= \a -> f ('#':rs) xs >>= \b -> pure (a + b)
    ways' f s (x:rx) | length s >= x && (notElem '.'.nub) (take x s) && notAfter x '#' s
      = f (drop (x + 1) s) rx
    ways' _ _ _= pure 0

notAfter :: Int -> Char -> String -> Bool
notAfter x c s = (notElem c . nub) (take 1 (drop x s))

_input="???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"
