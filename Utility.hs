module Utility
( distance
, line_side
, sign
) where

import Graphics.Gloss.Data.Vector

import Debug

sqr :: Float -> Float
sqr x = x * x

distance :: Vector -> Vector -> Float
distance (x, y) (x', y') = sqrt $ sqr (x - x') + sqr (y - y')

line_side :: Vector -> Float -> Vector -> Float
line_side (x1, y1) t (x2, y2) = let
  (m)    = (sin t) / (cos t)
  (x, y) = (x2 - x1, y2 - y1)
  in if (y == m * x) then 0
    else if (y > m * x) then -1
      else if (y < m * x) then 1
        else error $ "vector out of bounds: " ++ show (x2, y2)


sign :: Float -> Float
sign x
  | x == 0 = 0
  | x <  0 = -1
  | x >  0 = 1
