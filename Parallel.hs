module Parallel where

import Control.Parallel (par, pseq)

parMap :: (a -> b) -> [a] -> [b]
parMap f ls = case ls of
  []     -> []
  (x:xs) -> let
    r = f x
    in r `par` r : parMap f xs

forceList :: [a] -> ()
forceList ls = case ls of
  []     -> ()
  (x:xs) -> x `pseq` forceList xs

strictParMap :: (a -> b) -> [a] -> [b]
strictParMap f ls = forceList ls `seq` map f xs

forceListAndElts :: (a -> ()) -> [a] -> ()
forceListAndElts forceElt ls = case ls of
  []     -> ()
  (x:xs) -> forceElt x `seq` forceListAndElts forceElt xs


