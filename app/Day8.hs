module Day8 (run) where

import Combinators (both)
import Control.Arrow ((&&&), (***))
import Control.Monad.State
import Data.Function (on)
import Data.List (group, nub, sort, sortBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down (Down), comparing)
import Utils (chunk, split)

type JB = (Int, Int, Int)

data Env = Env
  { groups :: Map JB Int,
    p1Res, p2Res, iter, totalJuns :: Int
  }

nextIter :: State Env Int
nextIter = do
  i <- gets iter
  modify (\s -> s {iter = iter s - 1})
  return i

modifyGroups :: (Map JB Int -> Map JB Int) -> State Env ()
modifyGroups f = modify (\s -> s {groups = f (groups s)})

run :: String -> (String, String)
run = both show . part1and2 . parse
  where
    part1and2 = uncurry evalState . (calcConns *** buildInitState)
    buildInitState = Env M.empty 0 0 1000

calcConns :: [(JB, JB)] -> State Env (Int, Int)
calcConns ((jb, jb') : conns) = do
  gs <- gets groups
  it <- nextIter
  modify $ case it of
    x | x < 0 -> (\s -> s {p2Res = calcP2Res (jb, jb')})
    0 -> (\s -> s {p1Res = calcP1Res gs})
    _ -> id
  modifyGroups $ case both (`M.lookup` gs) (jb, jb') of
    (Just idx, Just idx')
      | idx /= idx' -> M.map (\i -> if i == idx' then idx else i)
      | otherwise -> id
    (Nothing, Just idx') -> M.insert jb idx'
    (Just idx, Nothing) -> M.insert jb' idx
    (Nothing, Nothing) -> M.insert jb it . M.insert jb' it
  gs' <- gets groups
  totalJuns <- gets totalJuns
  if length gs' == totalJuns && 1 == length (nub (M.elems gs'))
    then gets (p1Res &&& p2Res)
    else calcConns conns
  where
    calcP2Res = uncurry (*) . both (\(x, _, _) -> x)
    calcP1Res = product . take 3 . sortBy (comparing Down) . map length . group . sort . M.elems
calcConns [] = error "Invalid input"

parse :: String -> ([(JB, JB)], Int)
parse = (sortByDist &&& length) . parseJBs
  where
    parseJBs = map toTuple . chunk 3 . map read . concatMap (split ',') . lines
    sortByDist = sortBy (compare `on` dist) . connections
    dist ((a, b, c), (x, y, z)) = (a - x) * (a - x) + (b - y) * (b - y) + (c - z) * (c - z)
    toTuple [a, b, c] = (a, b, c)
    toTuple _ = error "Invalid input"
    connections [] = []
    connections (x : xs) = map (x,) xs ++ connections xs
