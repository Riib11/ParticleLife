module Environment
( Environment
, environment_size
, environment_size_int
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import Debug
import Particle

{-/----------------------------------------------------------------------------/
 /  Environment
/----------------------------------------------------------------------------/-}

-- TODO: add more parameters
data Environment = Environment
  { particles :: [Particle] }

environment_size     = (600, 600) :: (Float, Float)
environment_size_int = (600, 600) :: (Int, Int)
