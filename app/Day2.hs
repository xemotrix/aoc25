module Day2 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Utils (split)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

part1, part2 :: [(Int, Int)] -> Int
part1 = sum . (>>= findIDs isDouble)
part2 = sum . (>>= findIDs isRepeated)

findIDs :: (Int -> Bool) -> (Int, Int) -> [Int]
findIDs criteria (start, end) = filter criteria [start .. end]

isRepeated :: Int -> Bool
isRepeated n = any (($ nStr) . isMadeOf) patterns
  where
    nStr = show n
    patterns = map (`take` nStr) [1 .. length nStr `div` 2]
    isMadeOf pat num =
      concat (replicate (length num `div` length pat) pat) == num

isDouble :: Int -> Bool
isDouble n = take halfLen nStr == drop halfLen nStr
  where
    nStr = show n
    halfLen = length nStr `div` 2

parse :: String -> [(Int, Int)]
parse = map (both read . (head &&& last) . split '-') . split ','
