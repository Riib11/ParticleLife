module Input (input) where

import System.Exit
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

import Particle
import Environment
import Debug

input :: Event -> Environment -> IO Environment
input (EventKey (Char 'q') Down modifiers (x,y)) _ = do
  logged "stop ParticleLife"
  exitSuccess
input _ environment = return environment
