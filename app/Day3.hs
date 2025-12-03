module Day3 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

run :: String -> (String, String)
run = both show . (totalJoltage 2 &&& totalJoltage 12) . lines

totalJoltage :: Int -> [String] -> Int
totalJoltage n = sum . map (read . findJoltage n)

findJoltage :: Int -> String -> String
findJoltage 1 bats = [maximum bats]
findJoltage n bats = firstMax : findJoltage (n - 1) rest
  where
    firstMax = maximum beginning
    beginning = take (length bats - (n - 1)) bats
    rest = drop (restIdx + 1) bats
    restIdx = fromJust $ elemIndex firstMax bats
