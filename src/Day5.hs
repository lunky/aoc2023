module Day5
    (
    day5
   ,day5b
   ,_input
   , resolveLocation
   , resolveSeed
   , loadMap
    )
    where

import Data.List
import Data.List.Split
import qualified Data.Map as Map

day5 :: String -> Int
day5 input = minimum $ map (`resolveLocation` m) seeds
  where seeds = snd $ fst $ parseInput input
        m = loadMap input

day5b :: String -> Int
day5b input = head $ dropWhile (\y-> not $ inSet (resolveSeed y m) ) [1..]
  where seeds = snd $ fst $ parseInput input
        m = loadMap input
        ranges = chunksOf 2 seeds
        inSet = inRange ranges

inRange ranges x = any (\[r1,r2]-> x >=r1 && x < r1+r2) ranges

loadMap input = Map.fromList $ snd $ parseInput input


resolveLocation number m =
    llookup m "humidity-to-location map"
    $ llookup m "temperature-to-humidity map"
    $ llookup m "light-to-temperature map"
    $ llookup m "water-to-light map"
    $ llookup m "fertilizer-to-water map"
    $ llookup m "soil-to-fertilizer map"
    $ llookup m "seed-to-soil map" number

resolveSeed seed m =
    rlookup m "seed-to-soil map"
    $ rlookup m "soil-to-fertilizer map"
    $ rlookup m "fertilizer-to-water map"
    $ rlookup m "water-to-light map"
    $ rlookup m "light-to-temperature map"
    $ rlookup m "temperature-to-humidity map"
    $ rlookup m "humidity-to-location map" seed

llookup m prefix key = case inMap key of
                          Nothing -> key
                          Just [d,s,r] -> key - s + d
  where rules = (Map.!) m prefix
        inMap k = find (\[dest,source,range] -> k >= source && k <= source + range ) rules

rlookup m prefix key = case inMap key of
                          Nothing -> key
                          Just [d,s,r] -> key + s - d
  where rules = (Map.!) m prefix
        inMap k = find (\[dest,source,range] -> k >= dest && k <= dest + range ) rules

parseInput input = (seeds,maps)
  where seeds = (\[a,b]->(a,map (\y->read y::Int) $ words b)) $ splitOn ":" $ takeWhile (/='\n') input
        maps = drop 1
          $ map ((\[a,b]->(a,chunksOf 3 $ map (\y->read y::Int) $ words b)) . splitOn ":")
          $ splitOn "\n\n" input

_input="seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"
