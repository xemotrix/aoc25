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

data State = State
  { getGroups :: Map JB Int,
    getP1Res, getP2Res, getIter, getConnIdx, getTotalJuns :: Int
  }
  deriving (Show)

run :: String -> (String, String)
run = both show . part1and2 . parse
  where
    part1and2 = (getP1Res &&& getP2Res) . uncurry (calcConns . State M.empty 0 0 1000 0)

calcConns :: State -> [(JB, JB)] -> State
calcConns s ((jb, jb') : conns) =
  case getIter s of
    -1 ->
      if isAllConnected
        then s {getP2Res = calcP2Res (jb, jb')}
        else calcConns newS conns
    0 -> calcConns (s {getIter = -1, getP1Res = calcP1Res $ getGroups s}) conns
    _ -> calcConns (newS {getIter = getIter s - 1}) conns
  where
    calcP1Res = product . take 3 . sortBy (comparing Down) . map length . group . sort . M.elems
    calcP2Res = uncurry (*) . both (\(x, _, _) -> x)
    isAllConnected = length (getGroups newS) == getTotalJuns newS && 1 == length (nub (M.elems (getGroups newS)))
    newS = case both (`M.lookup` getGroups s) (jb, jb') of
      (Just idx, Just idx') | idx == idx' -> s
      (Just idx, Just idx') ->
        s
          { getGroups = M.map (\i -> if i == idx' then idx else i) (getGroups s)
          }
      (Nothing, Just idx') ->
        s
          { getGroups = M.insert jb idx' (getGroups s)
          }
      (Just idx, Nothing) ->
        s
          { getGroups = M.insert jb' idx (getGroups s)
          }
      (Nothing, Nothing) ->
        s
          { getGroups = M.insert jb (getConnIdx s) $ M.insert jb' (getConnIdx s) (getGroups s),
            getConnIdx = getConnIdx s + 1
          }
calcConns _ [] = error "Invalid input"

parse :: String -> (Int, [(JB, JB)])
parse = (length &&& sortByDist) . parseJBs
  where
    parseJBs = map toTuple . chunk 3 . map read . concatMap (split ',') . lines
    sortByDist = sortBy (compare `on` dist) . connections
    dist ((a, b, c), (x, y, z)) = (a - x) * (a - x) + (b - y) * (b - y) + (c - z) * (c - z)
    toTuple [a, b, c] = (a, b, c)
    toTuple _ = error "Invalid input"
    connections [] = []
    connections (x : xs) = map (x,) xs ++ connections xs
