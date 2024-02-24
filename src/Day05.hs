{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day05 (module Day05) where

import Data.Char (isDigit)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe

day5 :: IO ()
day5 = readFile "inputs/day05.txt" >>= print . lines

data LineMap = LineMap {destination, source, mapLength :: Int} deriving (Show)

data Almanac = Almanac
  { seeds :: [Int],
    seedToSoil,
    soilToFertilizer,
    seedToWater,
    waterToLight,
    lightToTemperature,
    tempToHumidity,
    humidityToLocation ::
      Map Int Int
  }

lineMapToTotalMap :: LineMap -> Map Int Int -> Map Int Int
lineMapToTotalMap (LineMap _ _ 0) m = m
lineMapToTotalMap (LineMap d s l) m = lineMapToTotalMap (LineMap (d + 1) (s + 1) (l - 1)) (insert s d m)

lineMapsToTotalMaps :: [LineMap] -> Map Int Int -> Map Int Int
lineMapsToTotalMaps xs m = foldl (flip lineMapToTotalMap) m xs

lookUpInTotalMap :: Int -> Map Int Int -> Int
lookUpInTotalMap x m
  | isNothing y = x
  | otherwise = (\(Just i) -> i) y
  where
    y = Data.Map.lookup x m

parseInts :: String -> [Int]
parseInts = map read . words . filter (\c -> isDigit c || c == ' ')

parseLineMap :: String -> LineMap
parseLineMap s = LineMap (head x) (head $ tail x) (last x)
  where
    x = map read . words . filter (\c -> isDigit c || c == ' ') $ s

isLineMap :: String -> Bool
isLineMap s = length (parseInts s) == 3

parseLinesToMaps :: [String] -> [LineMap] -> [Map Int Int] -> [Map Int Int]
parseLinesToMaps [] _ m = m
parseLinesToMaps [s] lm m = (++) m [lineMapsToTotalMaps ((++) lm [parseLineMap s]) Data.Map.empty]
parseLinesToMaps (s : ss) lm m
  | isLineMap s = parseLinesToMaps ss ((++) lm [parseLineMap s]) m
  | not $ isLineMap s = parseLinesToMaps ss lm ((++) m [lineMapsToTotalMaps lm Data.Map.empty])

input =
  [ "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "water-to-light map:",
    "88 18 7",
    "18 25 70"
  ]