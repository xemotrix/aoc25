module Day8 (run) where

import Combinators (both)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (group, nub, sort, sortBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down (Down), comparing)
import Utils (chunk, split)

type JB = (Int, Int, Int)

nConns :: Int
nConns = 1000

run :: String -> (String, String)
run = both show . part1and2 . parse

part1and2 :: ([(JB, JB)], Int) -> (Int, Int)
part1and2 (connsSorted, nJunBs) = (p1Res', p2Res')
  where
    (p1Res, p2Res) = calcConns nConns nJunBs 0 connsSorted M.empty M.empty
    p1Res' = product $ take 3 $ sortBy (comparing Down) $ map length $ group $ sort $ M.elems p1Res
    p2Res' = uncurry (*) $ both (\(x, _, _) -> x) p2Res

combinations :: [JB] -> [(JB, JB)]
combinations [] = []
combinations (x : xs) = map (x,) xs ++ combinations xs

calcConns :: Int -> Int -> Int -> [(JB, JB)] -> Map JB Int -> Map JB Int -> (Map JB Int, (JB, JB))
calcConns n totalJuns count ((jb, jb') : conns) accM p1Res =
  case n of
    -1 ->
      if length newM == totalJuns && 1 == length (nub (M.elems newM))
        then (p1Res, (jb, jb'))
        else calcConns (-1) totalJuns newCount conns newM p1Res
    0 -> calcConns (-1) totalJuns newCount conns newM newM
    _ -> calcConns (n - 1) totalJuns newCount conns newM p1Res
  where
    (newCount, newM) = case (M.lookup jb accM, M.lookup jb' accM) of
      (Just idx, Just idx') | idx == idx' -> (count, accM)
      (Just idx, Just idx') -> (count, M.map (\i -> if i == idx' then idx else i) accM)
      (Nothing, Just idx') -> (count, M.insert jb idx' accM)
      (Just idx, Nothing) -> (count, M.insert jb' idx accM)
      (Nothing, Nothing) -> (count + 1, M.insert jb count $ M.insert jb' count accM)
calcConns _ _ _ [] _ _ = error "Invalid input"

parse :: String -> ([(JB, JB)], Int)
parse = (sortByDist &&& length) . parseJBs
  where
    parseJBs = map toTuple . chunk 3 . map read . concatMap (split ',') . lines
    sortByDist = sortBy (compare `on` dist) . combinations
    dist ((a, b, c), (x, y, z)) = (a - x) * (a - x) + (b - y) * (b - y) + (c - z) * (c - z)
    toTuple [a, b, c] = (a, b, c)
    toTuple _ = error "Invalid input"
