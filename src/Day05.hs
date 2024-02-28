module Day05 (module Day05) where

import Control.Arrow
import Data.Char (isDigit)
import Data.List
import qualified Data.Maybe

day5 :: IO ()
day5 = readFile "inputs/day05.txt" >>= print . parseText . lines

data LineMap = LineMap {destination, source, mapLength :: Int} deriving (Show)

type RangeMap = ((Int, Int), Int)

type RangeMapTwo = (Int, Int, Int) -- (destination start, source start, range length)

data Almanac = Almanac
  { seeds :: [Int],
    seedToSoil,
    soilToFertilizer,
    fertilizerToWater,
    waterToLight,
    lightToTemperature,
    tempToHumidity,
    humidityToLocation ::
      [RangeMap]
  }
  deriving (Show)

lineMapToTotalMap :: LineMap -> [RangeMap] -> [RangeMap]
lineMapToTotalMap (LineMap d s l) x = (++) x [((s, s + (l - 1)), d - s)]

lineMapsToTotalMaps :: [LineMap] -> [RangeMap] -> [RangeMap]
lineMapsToTotalMaps xs mappers =
  foldl
    (\mappers x -> (++) mappers $ lineMapToTotalMap x [])
    mappers
    xs

lookUpInTotalMap :: Int -> [RangeMap] -> Int
lookUpInTotalMap x [] = x
lookUpInTotalMap x (((start, stop), diff) : rms)
  | x >= start && x <= stop = x + diff
  | otherwise = lookUpInTotalMap x rms

parseInts :: String -> [Int]
parseInts = map read . words . filter (\c -> isDigit c || c == ' ')

parseSeedRanges :: [Int] -> [(Int, Int)] -> [(Int, Int)]
parseSeedRanges [] r = r
parseSeedRanges (x : y : zs) r = parseSeedRanges zs ((++) r [(x, y)])
parseSeedRanges _ r = r

parseLineMap :: String -> LineMap
parseLineMap s = LineMap (head x) (head $ tail x) (last x)
  where
    x = map read . words . filter (\c -> isDigit c || c == ' ') $ s

isLineMap :: String -> Bool
isLineMap s = length (parseInts s) == 3

parseLinesToMaps :: [String] -> [LineMap] -> [[RangeMap]] -> [[RangeMap]]
parseLinesToMaps [] _ m = filter (not . null) m
parseLinesToMaps [s] lm m = filter (not . null) $ (++) m [lineMapsToTotalMaps ((++) lm [parseLineMap s]) []]
parseLinesToMaps (s : ss) lm m
  | isLineMap s = parseLinesToMaps ss ((++) lm [parseLineMap s]) m
  | not $ isLineMap s = parseLinesToMaps ss [] ((++) m [lineMapsToTotalMaps lm []])
parseLinesToMaps _ _ _ = []

parseText :: [String] -> Maybe Almanac
parseText (x : xs) = dataToAlmanac (parseInts x) $ parseLinesToMaps xs [] []
parseText _ = Nothing

dataToAlmanac :: [Int] -> [[RangeMap]] -> Maybe Almanac
dataToAlmanac seeds' [soilMap, fertilizerMap, waterMap, lightMap, temperatureMap, humidityMap, locationMap] =
  Just $
    Almanac
      { seeds = seeds',
        seedToSoil = soilMap,
        soilToFertilizer = fertilizerMap,
        fertilizerToWater = waterMap,
        waterToLight = lightMap,
        lightToTemperature = temperatureMap,
        tempToHumidity = humidityMap,
        humidityToLocation = locationMap
      }
dataToAlmanac _ _ = Nothing

part1 :: Maybe Almanac -> Maybe Int
part1 Nothing = Nothing
part1 (Just (Almanac seeds sTS sTF fTW wTL lTT tTH hTL)) = Just (almanacToLowestLocation seeds [sTS, sTF, fTW, wTL, lTT, tTH, hTL])

part2 :: Maybe Almanac -> Maybe Int
part2 Nothing = Nothing
part2 (Just (Almanac seeds sTS sTF fTW wTL lTT tTH hTL)) = undefined

almanacToLowestLocation :: [Int] -> [[RangeMap]] -> Int
almanacToLowestLocation ints [] = minimum ints
almanacToLowestLocation ints (m : ms) = almanacToLowestLocation (mapInts ints [] m) ms

mapInts :: [Int] -> [Int] -> [RangeMap] -> [Int]
mapInts [] l _ = l
mapInts (x : xs) l m = mapInts xs ((++) l [lookUpInTotalMap x m]) m

getRangesSeedRangeIntersects :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getRangesSeedRangeIntersects (seedStart, seedEnd) = filter (seedInRangeMapConversion seedStart seedEnd)

seedInRangeMapConversion :: Int -> Int -> (Int, Int) -> Bool
seedInRangeMapConversion seedStart seedEnd (rangeStart, rangeEnd) =
  not ((seedStart < rangeStart && seedEnd < rangeStart) || (seedStart > rangeEnd && seedEnd > rangeEnd))