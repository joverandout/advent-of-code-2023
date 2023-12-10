module Day01 (day1) where

import Control.Arrow
import Data.Char (digitToInt, isDigit)
import Lib
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

type Input = [String]

day1 :: IO ()
day1 = readFile "inputs/day01.txt" >>= print . (part1 &&& part2) . lines

part1 :: Input -> Int
part1 x = sum $ map callibration x

part2 :: Input -> Int
part2 x = sum $ map callibrationTwo x

callibrationTwo :: String -> Int
callibrationTwo line = 10 * getFirstDigitPartTwo line + getLastDigitPartTwo line

callibration :: String -> Int
callibration line = 10 * getFirstDigit line + getLastDigit line

getFirstDigit :: String -> Int
getFirstDigit line = digitToInt $ head $ filter isDigit line

getLastDigit :: String -> Int
getLastDigit line = digitToInt $ last $ filter isDigit line

getFirstDigitPartTwo :: String -> Int
getFirstDigitPartTwo line
  | isDigit $ head line = digitToInt $ head line
  | line =~ "^one" = 1
  | line =~ "^two" = 2
  | line =~ "^three" = 3
  | line =~ "^four" = 4
  | line =~ "^five" = 5
  | line =~ "^six" = 6
  | line =~ "^seven" = 7
  | line =~ "^eight" = 8
  | line =~ "^nine" = 9
  | otherwise = getFirstDigitPartTwo $ tail line

getLastDigitPartTwo :: String -> Int
getLastDigitPartTwo line
  | isDigit $ last line = digitToInt $ last line
  | line =~ "one$" = 1
  | line =~ "two$" = 2
  | line =~ "three$" = 3
  | line =~ "four$" = 4
  | line =~ "five$" = 5
  | line =~ "six$" = 6
  | line =~ "seven$" = 7
  | line =~ "eight$" = 8
  | line =~ "nine$" = 9
  | otherwise = getLastDigitPartTwo $ init line
