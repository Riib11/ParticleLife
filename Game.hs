module Game
( start
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import Debug
import Utility
import Particle
import Environment
import Display
import Input

{-/----------------------------------------------------------------------------/
 /  Play
/----------------------------------------------------------------------------/-}

start :: IO ()
start = play
  window
  background_color
  rate
  inital_conditions
  render
  input
  update

{-/----------------------------------------------------------------------------/
 /  Initial Conditions
/----------------------------------------------------------------------------/-}

inital_conditions :: Environment
inital_conditions = let
  ps = make_particles
    [ ((50, 50), 0)
    , ((50, 50), 0)
    , ((50, 50), 0)
    , ((50, 50), 0) ]
  in
    Environment
      (ps         :: [P])    -- particles
      (0.0        :: Float)  -- alpha
      (1.0        :: Float)  -- beta
      (1.0        :: Float)  -- rho
      ((100, 100) :: Vector) -- size

{-/----------------------------------------------------------------------------/
 /  Update
/----------------------------------------------------------------------------/-}

update :: Float -> Environment -> Environment
update dt environment = let
  
  alpha = environment_alpha environment
  beta  = environment_beta  environment
  rho   = environment_rho   environment
  particles = map reset_particle $ environment_particles environment
  
  -- update all the particles (each pair-wise interaction)
  update_neighbors :: [P] -> [P]
  update_neighbors []     = []
  update_neighbors (p:ps) = let
    (p', ps') = update_with_list p ps
    in p' : update_neighbors ps'

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

  -- after updating neighbor counts,
  -- can linearly update orientation by this rule for each particle:
  -- $$ dr/dt = alpha + beta * ns * sign(rns - lns) $$
  update_orientations :: [P] -> [P]
  update_orientations [] = []
  update_orientations (p:ps) = let
    (lns, rns) = particle_left_right_neighbors p
    (side, ns) = (sign (toFloat $ rns - lns), toFloat $ lns + rns)
    in add_orientation (alpha + (beta * ns * side)) p
       : update_orientations ps

  new_particles = update_orientations $ update_neighbors particles

  in set_particles new_particles environment

toFloat :: Int -> Float
toFloat = fromIntegral

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
reset_particle :: P -> P
reset_particle = error "unimplemented"
  -- (Particle uid pos ori lns rns) =
  -- (Particle uid pos ori 0   0)

-- increment neighbor count for the side
-- indicated by the given direction
inc_neighbors :: Float -> P -> P
inc_neighbors d p
  | d == 0 = p
  | d <  0 = add_left_neighbors  1 p
  | d >  0 = add_right_neighbors 1 p
