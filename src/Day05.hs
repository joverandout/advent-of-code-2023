{-# LANGUAGE ApplicativeDo #-}

module Day05 (module Day05) where

import Data.Char (GeneralCategory (NotAssigned), isDigit)
import Data.Map (Map, empty, insert, lookup, size)
import Data.Maybe

day5 :: IO ()
day5 = readFile "inputs/day05.txt" >>= print . parseText . lines

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
  deriving (Show)

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
parseLinesToMaps [] _ m = filter (/= empty) m
parseLinesToMaps [s] lm m = filter (/= empty) $ (++) m [lineMapsToTotalMaps ((++) lm [parseLineMap s]) Data.Map.empty]
parseLinesToMaps (s : ss) lm m
  | isLineMap s = parseLinesToMaps ss ((++) lm [parseLineMap s]) m
  | not $ isLineMap s = parseLinesToMaps ss [] ((++) m [lineMapsToTotalMaps lm Data.Map.empty])
parseLinesToMaps _ _ _ = []

parseText :: [String] -> Maybe Almanac
parseText (x : xs) = dataToAlmanac (parseInts x) $ parseLinesToMaps xs [] []
parseText _ = Nothing

dataToAlmanac :: [Int] -> [Map Int Int] -> Maybe Almanac
dataToAlmanac seeds' [soilMap, fertilizerMap, waterMap, lightMap, temperatureMap, humidityMap, locationMap] =
  Just $
    Almanac
      { seeds = seeds',
        seedToSoil = soilMap,
        soilToFertilizer = fertilizerMap,
        seedToWater = waterMap,
        waterToLight = lightMap,
        lightToTemperature = temperatureMap,
        tempToHumidity = humidityMap,
        humidityToLocation = locationMap
      }
dataToAlmanac _ _ = Nothing