module Day6 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.List (transpose)
import Utils (split)

run :: String -> (String, String)
run = both show . (part1 &&& part2)

part1, part2 :: String -> Int
part1 = sum . map calcOps . transpose . map words . lines
  where
    calcOps = ($) <$> opFun . last <*> map read . init
part2 = sum . (zipWith ($) <$> parseOps . last <*> parseNums . init) . lines
  where
    parseNums = map (map read . concat) . split [] . map words . transpose
    parseOps = map opFun . words

opFun :: String -> [Int] -> Int
opFun "+" = sum
opFun "*" = product
opFun _ = error "Unknown operator"
