module Environment
( Environment (Environment, environment_particles, environment_alpha, environment_beta, environment_rho, environment_size)
, set_particles
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import Debug
import Particle

{-/----------------------------------------------------------------------------/
 /  Environment
/----------------------------------------------------------------------------/-}

data Environment = Environment
  { environment_particles :: [P]    -- inhabiting particles
  , environment_alpha     :: Float  -- rotate alpha each update
  , environment_beta      :: Float  -- coefficient of interaction
  , environment_rho       :: Float  -- radius of interaction
  , environment_size      :: Vector -- size in sim units
  } deriving (Show)

set_particles :: [Particle] -> Environment -> Environment
set_particles ps
  (Environment _  alpha beta rho size) =
  (Environment ps alpha beta rho size)
