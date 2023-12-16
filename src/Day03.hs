{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Day03 (module Day03) where

import Data.Char (isDigit)
import Data.List (nub)
import Data.Map (Map (), member)
import qualified Data.Map.Strict as Map

data Coordinate = Coordinate {x, y :: Int} deriving (Show, Eq, Ord)

data Input = Map Coordinate Bool deriving (Show)

day3 :: IO ()
day3 = readFile "inputs/day03.txt" >>= print . part1 . lines

parseTokensFromLine :: Int -> Int -> String -> [(Coordinate, Bool)]
parseTokensFromLine _ _ [] = []
parseTokensFromLine y pos (x : xs)
  | x == '.' || isDigit x = parseTokensFromLine y (pos + 1) xs
  | otherwise = (Coordinate pos y, True) : parseTokensFromLine y (pos + 1) xs

parseTokensFromLines :: [String] -> Int -> [(Coordinate, Bool)]
parseTokensFromLines [] _ = []
parseTokensFromLines (x : xs) y = parseTokensFromLine y 1 x ++ parseTokensFromLines xs (y + 1)

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

extractGridCoordinates :: [String] -> Int -> [(Int, [Coordinate])]
extractGridCoordinates [] _ = []
extractGridCoordinates (x : xs) y = extractRowCoordinates x y ++ extractGridCoordinates xs (y + 1)

extractRowCoordinates :: String -> Int -> [(Int, [Coordinate])]
extractRowCoordinates s yPos = extraction s yPos 1 []
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
        newList num = numsAndPos ++ [(num, getCoordinates num xPos yPos)]

getCoordinates :: Int -> Int -> Int -> [Coordinate]
getCoordinates num xPos yPos = getNeighboursFromRange [Coordinate x yPos | x <- [xPos .. xPos + length (show num) - 1]]

integerBordersSymbol :: [Coordinate] -> Map Coordinate Bool -> Bool
integerBordersSymbol xs hashMap = foldr (\x y -> member x hashMap || y) False xs

addUpNumbersBorderingSymbol :: [(Int, [Coordinate])] -> Map Coordinate Bool -> Int -> Int
addUpNumbersBorderingSymbol [] _ total = total
addUpNumbersBorderingSymbol ((int, coords) : xs) symMap total
  | integerBordersSymbol coords symMap = addUpNumbersBorderingSymbol xs symMap (total + int)
  | otherwise = addUpNumbersBorderingSymbol xs symMap total

part1 :: [String] -> Int
part1 xs = addUpNumbersBorderingSymbol (extractGridCoordinates xs 1) (parseTokensToMap xs) 0
