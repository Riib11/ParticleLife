module Sorting where

import Control.Parallel (par, pseq)

-- sort
sort :: Ord a => [a] -> [a]
sort ls = case ls of
  []     -> []
  (x:xs) -> let
    lesser  = sort [y | y <- xs, y <  x]
    greater = sort [y | y <- xs, y >= x]
    in lesser ++ x:greater

-- parallel sort
parSort :: Ord a => [a] -> [a]
parSort ls = case ls of
  [] -> []
  (x:xs) -> let
    lesser  = parSort [y | y <- xs, y <  x]
    greater = parSort [y | y <- xs, y >= x]
    in par (force greater)
     $ pseq (force lesser)
     $ lesser ++ x:greater

-- force list elements to be evaluated
force :: [a] -> ()
force ls = let
  go []     = 1
  go (_:xs) = go xs
  in go ls `pseq` ()
