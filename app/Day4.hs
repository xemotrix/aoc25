module Day4 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.Set (Set, (\\))
import Data.Set qualified as S

run :: String -> (String, String)
run = both show . (part1 &&& part2) . parse

part1, part2 :: Set (Int, Int) -> Int
part1 = length . removable
part2 s =
  if S.null remo
    then 0
    else length remo + part2 (s \\ remo)
  where
    remo = removable s

removable :: Set (Int, Int) -> Set (Int, Int)
removable s = S.filter ((< 4) . length . filter (`S.member` s) . neigs) s

neigs :: (Int, Int) -> [(Int, Int)]
neigs (y, x) =
  [ (y - 1, x - 1),
    (y - 1, x),
    (y - 1, x + 1),
    (y, x - 1),
    (y, x + 1),
    (y + 1, x - 1),
    (y + 1, x),
    (y + 1, x + 1)
  ]

parse :: String -> Set (Int, Int)
parse input =
  S.fromList
    [ (y, x)
      | (y, xs) <- zip [0 ..] (lines input),
        (x, '@') <- zip [0 ..] xs
    ]
