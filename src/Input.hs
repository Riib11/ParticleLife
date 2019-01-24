module Input (input) where

import System.Exit
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

import Debug

import Particle
import Environment

input :: Event -> Environment -> IO Environment
input (EventKey (Char 'q') Down modifiers (x,y)) _ = do
  logged "stop ParticleLife"
  exitSuccess
input _ environment = return environment
