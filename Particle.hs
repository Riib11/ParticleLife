module Particle
( Particle (Particle, particle_uid, particle_position, particle_orientation, particle_left_neighbors, particle_right_neighbors), P
, make_particle, make_particles
, particle_left_right_neighbors
, add_left_neighbors, add_right_neighbors
, add_orientation
, set_position
, particle_neighbors
) where

import Control.Parallel
import Control.Parallel.Strategies
-- import Control.DeepSeq

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

import Debug

data Particle = Particle
  { particle_uid             :: Int
  , particle_position        :: Vector
  , particle_orientation     :: Float
  , particle_left_neighbors  :: Int
  , particle_right_neighbors :: Int }
  deriving (Show)
type P = Particle

make_particle :: Int -> Vector -> Float -> Particle
make_particle uid pos ori = Particle uid pos ori 0 0

make_particles :: [(Vector, Float)] -> [Particle]
make_particles = let
  helper i [] = []
  helper i ((v,r):xs) = make_particle i v r : helper (i + 1) xs
  in helper 0

particle_left_right_neighbors :: P -> (Int, Int)
particle_left_right_neighbors p =
  (particle_left_neighbors p, particle_right_neighbors p)

add_left_neighbors :: Int -> P -> P
add_left_neighbors dn
  (Particle uid pos ori lns        rns) =
  (Particle uid pos ori (lns + dn) rns)

add_right_neighbors :: Int -> P -> P
add_right_neighbors dn
  (Particle uid pos ori lns (rns)     ) =
  (Particle uid pos ori lns (rns + dn))

add_orientation :: Float -> P -> P
add_orientation dr
  (Particle uid pos (ori)      lns rns) =
  (Particle uid pos (ori + dr) lns rns)

set_position :: Vector -> P -> P
set_position (x, y)
  (Particle uid pos    ori lns rns) =
  (Particle uid (x, y) ori lns rns)

particle_neighbors :: P -> Int
particle_neighbors p = let
  (lns, rns) = particle_left_right_neighbors p
  in lns + rns
