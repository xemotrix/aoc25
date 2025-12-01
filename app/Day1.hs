module Day1 (run) where

import Combinators (both)
import Control.Arrow ((&&&))

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

part1, part2 :: [Int] -> Int
part1 = length . filter ((==0) . (`mod` 100))
part2 =  sum . map count0s . (zip <*> tail)

count0s :: (Int, Int) -> Int
count0s (n, n') =  abs $ offsetStart + offsetEnd + (div n' 100) - (div n 100)
  where
    offsetStart = if n `mod` 100 == 0 && (n' < n) then 1 else 0
    offsetEnd = if n' `mod` 100 == 0 && (n' < n) then -1 else 0

parse :: String -> [Int]
parse = scanl (+) 50 . map parseLine . lines
  where 
    parseLine (sign : num) = parseSign sign * read num
    parseSign 'R' = 1
    parseSign 'L' = -1
