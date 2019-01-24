module Environment
( Environment (Environment, environment_velocity, environment_particles, environment_alpha, environment_beta, environment_rho, environment_size)
, set_particles
) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

import Debug

import Particle

{-/----------------------------------------------------------------------------/
 /  Environment
/----------------------------------------------------------------------------/-}

data Environment = Environment
  { environment_particles :: [P]    -- inhabiting particles
  , environment_velocity  :: Float  -- units moved each update
  , environment_alpha     :: Float  -- rotate alpha each update
  , environment_beta      :: Float  -- coefficient of interaction
  , environment_rho       :: Float  -- radius of interaction
  , environment_size      :: Vector -- size in sim units
  } deriving (Show)

set_particles :: [Particle] -> Environment -> Environment
set_particles ps
  (Environment _  v a b r size) =
  (Environment ps v a b r size)
