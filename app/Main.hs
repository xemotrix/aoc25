module Main where

import System.Environment (getArgs)
import Day1

type Day = (Int, String -> (String, String))

days :: [Day]
days =
  zip
    [1 ..]
    [ Day1.run ]

main :: IO ()
main = do
  args <- getArgs
  let days' = case args of
        [] -> days
        [daynum] -> filter ((== read daynum) . fst) days
        _ -> error "usage: advent-of-code [day]"
  mapM_ runDay days'

runDay :: Day -> IO ()
runDay (daynum, dayf) = do
  input <- readInput daynum
  putStrLn $ fmtDay $ dayf input
  where
    fmtDay (part1, part2) =
      "day "
        ++ show daynum
        ++ "\n\tpart 1: "
        ++ part1
        ++ "\n\tpart 2: "
        ++ part2
    readInput n = readFile $ "./inputs/input" ++ show n ++ ".txt"
