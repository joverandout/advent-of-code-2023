{-# LANGUAGE ApplicativeDo #-}

module Day04 (module Day04) where

import Control.Applicative (Alternative (some))
import Data.Maybe (mapMaybe)
import Lib (seperated)
import Text.Regex.Applicative (RE, string, sym, (=~))
import Text.Regex.Applicative.Common (decimal)

day4 :: IO ()
day4 = readFile "inputs/day04.txt" >>= print . part1 . mapMaybe (=~ parseCard) . lines

data Card = Card {dealt, winners :: [Int]} deriving (Show, Eq, Ord)

parseSet :: RE Char [Int]
parseSet = seperated decimal $ some $ string " "

parseCard :: RE Char Card
parseCard =
  Card
    <$> ( (string "Card" *> some (sym ' ') *> decimal)
            *> sym ':'
            *> some (sym ' ')
            *> parseSet
        )
    <*> (some (sym ' ') *> sym '|' *> some (sym ' ') *> parseSet)

getCardValue :: Card -> Int
getCardValue (Card d w)
  | exp >= 0 = 2 ^ exp
  | otherwise = 0
  where
    exp = length (d `intersect` w) - 1

intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect _ [] = []
intersect xs ys = filter (`elem` xs) ys

part1 :: [Card] -> Int
part1 cards = sum (map getCardValue cards)