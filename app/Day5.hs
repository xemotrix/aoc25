module Day5 (run) where

import Combinators (both)
import Control.Arrow (second, (&&&))
import Data.List (nub, sort)
import Utils (between, split)

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

part1, part2 :: ([(Int, Int)], [Int]) -> Int
part1 (ranges, ids) = length $ filter inAnyRange ids
  where
    inAnyRange = or . (map (uncurry between) ranges <*>) . pure
part2 (ranges, _) = length nums + sum (map innerRangeLen containedSteps)
  where
    nums = nub . sort $ allNums ranges
    allNums = concatMap $ uncurry (:) . second pure
    steps = zip <*> tail $ nums
    containedSteps = filter (containedInAny ranges) steps
    innerRangeLen (low, high) = high - low - 1

containedInAny :: [(Int, Int)] -> (Int, Int) -> Bool
containedInAny ranges x = any (contained x) ranges

contained :: (Int, Int) -> (Int, Int) -> Bool
contained (a, b) (c, d) = a >= c && b <= d

parse :: String -> ([(Int, Int)], [Int])
parse = (parseRanges &&& parseIds) . lines
  where
    parseRanges = map (both read . (head &&& last) . split '-') . takeWhile (/= "")
    parseIds = map read . reverse . takeWhile (/= "") . reverse
