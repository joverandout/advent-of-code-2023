{-# LANGUAGE ApplicativeDo #-}

module Day02 (module Day02) where

import Control.Arrow
import Data.Map (Map ())
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Lib (seperated)
import Text.Regex.Applicative (RE, string, sym, (<|>), (=~))
import Text.Regex.Applicative.Common (decimal)

day2 :: IO ()
day2 = readFile "inputs/day02.txt" >>= print . (part1 &&& part2) . mapMaybe (=~ parseGame) . lines

data Colour = Red | Green | Blue deriving (Show, Eq, Ord)

data Pick = Pick Int Colour deriving (Show)

data Pull = Pull [Pick] deriving (Show)

data PickKVP = PickKVP (Colour, Int) deriving (Show)

data Game = Game {id :: Int, pulls :: [Pull]} deriving (Show)

parseColour :: RE Char Colour
parseColour =
  Red <$ string "red"
    <|> Blue <$ string "blue"
    <|> Green <$ string "green"

parsePick :: RE Char Pick
parsePick = Pick <$> decimal <* sym ' ' <*> parseColour

parsePull :: RE Char Pull
parsePull = Pull <$> seperated parsePick (string ", ")

parsePulls :: RE Char [Pull]
parsePulls = seperated parsePull $ string "; "

parseGame :: RE Char Game
parseGame = Game <$> (string "Game " *> decimal) <* string ": " <*> parsePulls

pickToKVP :: Pick -> (Colour, Int)
pickToKVP (Pick n c) = (c, n)

picksToKVPs :: [Pick] -> [(Colour, Int)]
picksToKVPs = foldr (\x -> (++) [pickToKVP x]) []

pullToMap :: Pull -> Map Colour Int
pullToMap (Pull picks) = Map.fromList (picksToKVPs picks)

pullsToMap :: [Pull] -> Map Colour Int
pullsToMap = foldr (Map.unionWith max . pullToMap) Map.empty

gameToMap :: Game -> Map Colour Int
gameToMap (Game _ pulls) = pullsToMap pulls

isGameEligible :: Game -> Bool
isGameEligible game =
  getCountFromMaybe (Map.lookup Blue map) <= 14
    && getCountFromMaybe (Map.lookup Green map) <= 13
    && getCountFromMaybe (Map.lookup Red map) <= 12
  where
    map = gameToMap game

getCountFromMaybe :: Maybe Int -> Int
getCountFromMaybe (Just x) = x
getCountFromMaybe Nothing = 0

sumGames :: [Game] -> Int
sumGames [] = 0
sumGames ((Game id _) : xs) = id + sumGames xs

part1 :: [Game] -> Int
part1 games = sumGames (filter isGameEligible games)

part2 :: [Game] -> Int
part2 game = sum $ map (product . gameToMap) game