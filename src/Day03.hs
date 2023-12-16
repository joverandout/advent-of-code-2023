{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Day03 (module Day03) where

import Control.Arrow
import Data.Char (isDigit)
import Data.List (intersect, nub)
import Data.Map (Map (), member)
import qualified Data.Map.Strict as Map

data Coordinate = Coordinate {x, y :: Int} deriving (Show, Eq, Ord)

data Input = Map Coordinate Bool deriving (Show)

day3 :: IO ()
day3 = readFile "inputs/day03.txt" >>= print . (part1 &&& part2) . lines

parseTokensFromLine :: Int -> Int -> String -> [(Coordinate, Bool)]
parseTokensFromLine _ _ [] = []
parseTokensFromLine y pos (x : xs)
  | x == '.' || isDigit x = parseTokensFromLine y (pos + 1) xs
  | otherwise = (Coordinate pos y, True) : parseTokensFromLine y (pos + 1) xs

parseGearsFromLine :: Int -> Int -> String -> [Coordinate]
parseGearsFromLine _ _ [] = []
parseGearsFromLine y pos (x : xs)
  | x /= '*' = parseGearsFromLine y (pos + 1) xs
  | otherwise = Coordinate pos y : parseGearsFromLine y (pos + 1) xs

parseTokensFromLines :: [String] -> Int -> [(Coordinate, Bool)]
parseTokensFromLines [] _ = []
parseTokensFromLines (x : xs) y = parseTokensFromLine y 1 x ++ parseTokensFromLines xs (y + 1)

parseGearsFromLines :: [String] -> Int -> [Coordinate]
parseGearsFromLines [] _ = []
parseGearsFromLines (x : xs) y = parseGearsFromLine y 1 x ++ parseGearsFromLines xs (y + 1)

parseTokensToMap :: [String] -> Map Coordinate Bool
parseTokensToMap s = Map.fromList $ parseTokensFromLines s 1

getNeighbours :: Coordinate -> [Coordinate]
getNeighbours (Coordinate x y) =
  [Coordinate (x + dx) (y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

getNeighboursFromRange :: [Coordinate] -> [Coordinate]
getNeighboursFromRange cs = nub $ concatMap getNeighbours cs

sumValidNums :: Int -> [Coordinate] -> Int -> Map Coordinate Bool -> Int
sumValidNums num neighbours total tokens
  | coordsInMap neighbours tokens = total + num
  | otherwise = total

coordsInMap :: [Coordinate] -> Map Coordinate Bool -> Bool
coordsInMap cs map = foldr (\c -> (||) (member c map)) False cs

extractGridCoordinates :: [String] -> Int -> (Int -> Int -> Int -> [Coordinate]) -> [(Int, [Coordinate])]
extractGridCoordinates [] _ _ = []
extractGridCoordinates (x : xs) y getCoords = extractRowCoordinates x y getCoords ++ extractGridCoordinates xs (y + 1) getCoords

extractRowCoordinates :: String -> Int -> (Int -> Int -> Int -> [Coordinate]) -> [(Int, [Coordinate])]
extractRowCoordinates s yPos getCoords = extraction s yPos 1 []
  where
    extraction [] _ _ numsAndPos = numsAndPos
    extraction (c : cs) yPos xPos numsAndPos
      | isDigit c = case span isDigit (c : cs) of
          (numStr, rest@(c' : cs'))
            | not (isDigit c') ->
                let num = read numStr
                    newPos = xPos + length numStr - 1
                 in extraction rest yPos (newPos + 1) (newList num)
            | otherwise -> extraction cs' yPos (xPos + 1) numsAndPos
          (numStr, rest) ->
            let num = read numStr
                newPos = xPos + length numStr - 1
             in extraction rest yPos (newPos + 1) (newList num)
      | otherwise = extraction cs yPos (xPos + 1) numsAndPos
      where
        newList num = numsAndPos ++ [(num, getCoords num xPos yPos)]

getNeighbouringCoordinates :: Int -> Int -> Int -> [Coordinate]
getNeighbouringCoordinates num xPos yPos = getNeighboursFromRange [Coordinate x yPos | x <- [xPos .. xPos + length (show num) - 1]]

getCoordinates :: Int -> Int -> Int -> [Coordinate]
getCoordinates num xPos yPos = [Coordinate x yPos | x <- [xPos .. xPos + length (show num) - 1]]

addUpNumbersBorderingSymbol :: [(Int, [Coordinate])] -> Map Coordinate Bool -> Int -> Int
addUpNumbersBorderingSymbol [] _ total = total
addUpNumbersBorderingSymbol ((int, coords) : xs) symMap total
  | coordsInMap coords symMap = addUpNumbersBorderingSymbol xs symMap (total + int)
  | otherwise = addUpNumbersBorderingSymbol xs symMap total

numbersNextToGear :: Coordinate -> [(Int, [Coordinate])] -> [Int] -> [Int]
numbersNextToGear _ [] ints = ints
numbersNextToGear gear ((int, coords) : xs) ints
  | not (null (getNeighbours gear `intersect` coords)) = int : numbersNextToGear gear xs ints
  | otherwise = numbersNextToGear gear xs ints

getGearRatio :: Coordinate -> [(Int, [Coordinate])] -> Int
getGearRatio gear parts = case neighbourParts of
  [x, y] -> x * y
  _ -> 0
  where
    neighbourParts = numbersNextToGear gear parts []

getGearRatioSum :: [Coordinate] -> [(Int, [Coordinate])] -> Int
getGearRatioSum [] _ = 0
getGearRatioSum (gear : gears) parts = getGearRatio gear parts + getGearRatioSum gears parts

part1 :: [String] -> Int
part1 xs = addUpNumbersBorderingSymbol (extractGridCoordinates xs 1 getNeighbouringCoordinates) (parseTokensToMap xs) 0

part2 :: [String] -> Int
part2 xs = getGearRatioSum (parseGearsFromLines xs 1) (extractGridCoordinates xs 1 getCoordinates)