module Update
( update
) where

import Control.Parallel
import Control.Parallel.Strategies
import Parallel

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

import Debug
import Utility

import Particle
import Environment

{-/----------------------------------------------------------------------------/
 /  Update
/----------------------------------------------------------------------------/-}

update :: Float -> Environment -> IO Environment
update dt environment = let
  v     = environment_velocity  environment
  alpha = environment_alpha     environment
  beta  = environment_beta      environment
  rho   = environment_rho       environment
  ps    = environment_particles environment

  -- reset each particle's left/right neighbors to 0
  reset_neighbors :: [P] -> [P]
  reset_neighbors = parMap rseq p_reset_neighbors

  -- update neighbor counts for each particle
  update_neighbors :: [P] -> [P]

  -- update_neighbors []     = []
  -- update_neighbors (p:ps) = let
  --   (p', ps') = update_with_list p ps
  --   in p' : update_neighbors ps'

  update_neighbors ps = let
    -- particle-rest pairs
    p_ps_pairs []     = []
    p_ps_pairs (p:ps) = (p, ps) : p_ps_pairs ps
    -- map
    map_func :: (P, [P]) -> (P, [P])
    map_func (p, ps) = update_with_list p ps
    -- reduce
    red_func :: [(P, [P])] -> [P]
    red_func [] = []
    red_func ((p, ps):xs) = p : (ps_combine ps $ red_func xs)
    -- map-reduce, where
    --  a = (P, [P])
    --  b = (P, [P])
    --  c = [P]
    in parSimpleMapReduce map_func red_func (p_ps_pairs ps)

  -- update a particle with each of a list of particles
  update_with_list :: P -> [P] -> (P, [P])
  update_with_list p0 []     = (p0, [])
  update_with_list p0 (p:ps) = let
    (p0' , p' ) = update_pair p0 p
    (p0'', ps') = update_with_list p0' ps
    in (p0'', p':ps')

  -- update the neighbor counts for a pair of particles
  update_pair :: P -> P -> (P, P)
  update_pair p1 p2 = let
    helper p p' = case neighbor_status rho p p' of
      Nothing -> (p, p')
      Just d  -> (inc_neighbors d p, p')
    (p1' , p2' ) = helper p1  p2
    (p2'', p1'') = helper p2' p1'
    in (p1'', p2'')

  -- can linearly update orientation by this rule for each particle:
  -- $$ dr/dt = alpha + beta * ns * sign(rns - lns) $$
  update_orientations :: [P] -> [P]
  update_orientations [] = []
  update_orientations (p:ps) = let
    (lns, rns) = particle_left_right_neighbors p
    (side, ns) = (sign (toFloat $ rns - lns), toFloat $ lns + rns)
    in add_orientation (alpha + (beta * ns * side)) p
       : update_orientations ps

  -- can linearly update position by this rule for each particle:
  -- $$ dp/dt = v * (cos t, sin t)
  update_positions :: [P] -> [P]
  update_positions ps = let
    update_position p = let
      (x , y ) = particle_position p
      (ori)    = particle_orientation p
      (dx, dy) = (v * cos ori, v * sin ori)
      in set_position (x + dx, y + dy) p
    in map update_position ps

  new_particles
    = update_positions
    $ update_orientations
    $ update_neighbors
    $ reset_neighbors
      ps

  in return $ set_particles new_particles environment

-- combine the left/right neighbor counts of each particle pair
-- (with the name UID) in order between two particles lists
ps_combine :: [P] -> [P] -> [P]
ps_combine ps1 ps2 = case (ps1, ps2) of
  ([], []) -> []
  ((p1:ps1'), (p2:ps2')) -> p_combine p1 p2 : ps_combine ps1' ps2'

-- combine the left/right neighbor counts of two particles
-- with the same UID
p_combine :: P -> P -> P
p_combine p1 p2 = let
  uid = particle_uid         p1
  pos = particle_position    p1
  ori = particle_orientation p1
  (p1_lns, p1_rns) = particle_left_right_neighbors p1
  (p2_lns, p2_rns) = particle_left_right_neighbors p2
  in Particle uid pos ori (p1_lns + p2_lns) (p1_lns + p1_rns)

-- Nothing if not neighbor particle
-- Float (-1 if left, +1 if right)
neighbor_status :: Float -> P -> P -> Maybe Float
neighbor_status rho p p' = let
  -- neighbor particles are two particles within rho distance of each other
  is_neighbor :: P -> P -> Bool
  is_neighbor p p' = let
    v  = particle_position p
    v' = particle_position p'
    in distance v v' <= rho
  in if not (is_neighbor p p')
    then Nothing
    else let
      (pos, ori) = (particle_position p, particle_orientation p)
      (pos')     = (particle_position p')
      in Just $ line_side pos ori pos'

-- resets neighbors to 0
p_reset_neighbors :: P -> P
p_reset_neighbors
  (Particle uid pos ori lns rns) =
  (Particle uid pos ori 0   0)

-- increment neighbor count for the side
-- indicated by the given direction
inc_neighbors :: Float -> P -> P
inc_neighbors d p
  | d == 0 = p
  | d <  0 = add_left_neighbors  1 p
  | d >  0 = add_right_neighbors 1 p
