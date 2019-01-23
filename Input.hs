module Input (input) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import Particle
import Environment
import Debug

input :: Event -> Environment -> Environment
input (EventKey key Down modifiers (x,y)) world = error "unimplemented"
