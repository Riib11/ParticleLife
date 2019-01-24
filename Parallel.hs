module Parallel
( parMapReduce
, parSimpleMapReduce
) where

import Control.Parallel
import Control.Parallel.Strategies

-- parallelized map reduce
parMapReduce
  :: Strategy b     -- strategy for mapping
  -> (a -> b)       -- map function
  -> Strategy c     -- strategy for reducing
  -> ([b] -> c)     -- reduce function
  -> [a]
  -> c
parMapReduce map_strat map_func red_strat red_func ls = let
  map_result = parMap map_strat map_func ls
  red_result = red_func map_result `using` red_strat
  in map_result `pseq` red_result

parSimpleMapReduce
  :: (a -> b)       -- map function
  -> ([b] -> c)     -- reduce function
  -> [a]
  -> c
parSimpleMapReduce map_func red_func ls =
  parMapReduce rseq map_func rseq red_func ls
