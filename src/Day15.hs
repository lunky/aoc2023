module Day15
    (
    day15
   ,day15b
   ,_input
   ,_input2
   ,hash
    )
    where
import Data.Char
import Data.List
import Data.List.Split
import qualified  Text.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P
import Data.Either
import qualified Data.Map as Map

type Parser = Parsec.Parsec String ()

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

day15 :: String -> Int
day15 input = sum $ map hash $ parseInput input

{--

    Determine the ASCII code for the current character of the string.
    Increase the current value by the ASCII code you just determined.
    Set the current value to itself multiplied by 17.
    Set the current value to the remainder of dividing itself by 256.
--}

hash :: String -> Int
hash = foldl' (\acc c -> (`mod` 256) $ (*17) $ ord c + acc) 0

day15b :: String -> Int
day15b input = sum $ concatMap (\(day,lenses) -> 
                zipWith (\ a b -> product [a, 1 + day, (\ z -> read z :: Int) $ last (words b)]) [1..] (reverse lenses) ) 
                          $ Map.toList $ day15b' input

day15b' :: String -> Map.Map Int [String]
day15b' input = foldl' (\acc y -> case y of
                                  Add k v -> addIt k (k ++ " " ++ show v) acc
                                  Remove k -> deleteIt k acc
                                       ) Map.empty
                  $ rights $ map (parse parseOp) $  parseInput input

addIt :: String -> String -> Map.Map Int [String] -> Map.Map Int [String]
addIt k v m = case Map.lookup (hash k) m of
              Nothing -> Map.insert (hash k) [v] m
              Just s -> Map.insert (hash k) (if not (any (\a -> k `isPrefixOf` a) s)
                                            then v:s
                                            else map (\a -> if k `isPrefixOf` a then v else a) s  ) m

deleteIt :: String -> Map.Map Int [String] -> Map.Map Int [String]
deleteIt k m =  case Map.lookup (hash k) m of
                Nothing -> m
                Just s -> Map.insert (hash k) (filter (\a -> not (k `isPrefixOf` a)) s) m

parseInput input = splitOn "," $ concat $ lines input

data Op = Add String Int | Remove String deriving (Show,Eq)

parseAdd :: Parser Op
parseAdd = do
  key <- P.many1 (P.noneOf "=")
  _ <- P.string "="
  box <- P.many1 P.digit
  return (Add key (read box))

parseRemove :: Parser Op
parseRemove = do
  key <- P.many1 (P.noneOf "-")
  _ <- P.string "-"
  return $ Remove key

parseOp :: Parser Op
parseOp = do
  P.try parseRemove <|> P.try parseAdd

_input="HASH"
_input2="rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
