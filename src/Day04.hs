{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day04 (module Day04) where

import Control.Applicative (Alternative (some))
import Control.Arrow (Arrow (..))
import Data.Map (Map, adjust)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Lib (seperated)
import Text.Regex.Applicative (RE, string, sym, (=~))
import Text.Regex.Applicative.Common (decimal)

day4 :: IO ()
day4 = readFile "inputs/day04.txt" >>= print . (part1 &&& part2) . mapMaybe (=~ parseCard) . lines

data Card = Card {cardId :: Int, dealt, winners :: [Int]} deriving (Show, Eq, Ord)

data CardMap = Map Int (Card, Int) deriving (Show)

parseSet :: RE Char [Int]
parseSet = seperated decimal $ some $ string " "

parseCard :: RE Char Card
parseCard =
  Card
    <$> ( string "Card"
            *> some (sym ' ')
            *> decimal
        )
    <*> ( sym ':'
            *> some (sym ' ')
            *> parseSet
        )
    <*> ( some (sym ' ')
            *> sym '|'
            *> some (sym ' ')
            *> parseSet
        )

getCardValuePart1 :: Card -> Int
getCardValuePart1 (Card _ d w)
  | exp >= 0 = 2 ^ exp
  | otherwise = 0
  where
    exp = length (d `intersect` w) - 1

getCardValuePart2 :: Card -> Int
getCardValuePart2 (Card _ d w) = length (d `intersect` w)

intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect _ [] = []
intersect xs ys = filter (`elem` xs) ys

getId :: Card -> Int
getId (Card {cardId = cardId}) = cardId

cardsToMap :: [Card] -> Map Int (Card, Int)
cardsToMap = Map.fromList . map (\card -> (getId card, (card, 1)))

updateCard :: Int -> Int -> Map Int (Card, Int) -> Map Int (Card, Int)
updateCard increase = adjust (second (+ increase))

updateCards :: Int -> [Int] -> Map Int (Card, Int) -> Map Int (Card, Int)
updateCards _ [] map = map
updateCards increase (x : xs) map = updateCards increase xs (updateCard increase x map)

getCard :: Int -> Map Int (Card, Int) -> (Card, Int)
getCard id map = fromJust $ Map.lookup id map

oneUpdate :: Int -> Map Int (Card, Int) -> Map Int (Card, Int)
oneUpdate id map = updateCards (snd $ pair) (take (getCardValuePart2 $ fst pair) [(id + 1) ..]) map
  where
    pair = getCard id map

runMap :: [Int] -> Map Int (Card, Int) -> Map Int (Card, Int)
runMap xs map = foldl (flip oneUpdate) map xs

sumMap :: Int -> Int -> Map Int (Card, Int) -> Int
sumMap total 0 _ = total
sumMap total id map = snd (getCard id map) + sumMap total (id - 1) map

part1 :: [Card] -> Int
part1 cards = sum (map getCardValuePart1 cards)

part2 :: [Card] -> Int
part2 cards = sumMap 0 len $ runMap [1 .. len] $ cardsToMap cards
  where
    len = length cards