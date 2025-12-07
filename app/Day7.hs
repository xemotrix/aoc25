module Day7 (run) where

import Combinators (both)
import Control.Arrow (Arrow (first), (&&&))
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

part1, part2 :: (Set (Int, Int), (Int, Int), Int) -> Int
part1 (splitters, start, maxDepth) = length $ calcSplits splitters maxDepth (S.singleton start)
part2 (splitters, start, maxDepth) = calcTimelines splitters maxDepth (M.singleton start 1)

calcSplits :: Set (Int, Int) -> Int -> Set (Int, Int) -> Set (Int, Int)
calcSplits _ 0 _ = S.empty
calcSplits splitters depth starts = S.union visited (calcSplits splitters (depth - 1) next)
  where
    downs = S.map down starts
    next = S.fromList $ concatMap (maybeSplit splitters) downs
    visited = S.intersection splitters downs

calcTimelines :: Set (Int, Int) -> Int -> Map (Int, Int) Int -> Int
calcTimelines _ 0 starts = sum $ M.elems starts
calcTimelines splitters depth starts = calcTimelines splitters (depth - 1) next
  where
    downs = map (first (maybeSplit splitters . down)) $ M.toList starts
    flattened = concatMap (\(ps, n) -> map (,n) ps) downs
    next = M.fromListWith (+) flattened

maybeSplit :: Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
maybeSplit splitters pos =
  if pos `S.member` splitters
    then [left pos, right pos]
    else [pos]

left, right, down :: (Int, Int) -> (Int, Int)
left (y, x) = (y, x - 1)
right (y, x) = (y, x + 1)
down (y, x) = (y + 1, x)

parse :: String -> (Set (Int, Int), (Int, Int), Int)
parse input = (splitters, start, nlines)
  where
    nlines = length $ lines input
    start = (0, fromJust $ elemIndex 'S' $ head $ lines input)
    splitters =
      S.fromList
        [ (y, x)
          | (y, row) <- zip [0 ..] $ lines input,
            (x, '^') <- zip [0 ..] row
        ]
