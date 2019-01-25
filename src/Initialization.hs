module Initialization (initial_conditions) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

import Debug
import Utility

import Particle
import Environment

{-/----------------------------------------------------------------------------/
 /  Initial Conditions
/----------------------------------------------------------------------------/-}

pid2 = pi / 2 :: Float
radius = 50.0 :: Float

ps_stream = let
  stream i =
    make_particle i
      (radius * (cos $ toFloat i), radius * (sin $ toFloat i))
      (toFloat i)
    : stream (i + 1)
  in stream 0

initial_conditions :: Environment
initial_conditions = let
  ps = take 120 ps_stream
  in
    Environment
      (ps         :: [P])    -- particles
      (0.67       :: Float)  -- velocity
      (pid2       :: Float)  -- alpha
      (17.0       :: Float)  -- beta
      (5.0        :: Float)  -- rho
      ((100, 100) :: Vector) -- size