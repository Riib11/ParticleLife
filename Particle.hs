module Particle
( Particle
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import Debug

data Particle = Particle
  { position    :: Vector
  , orientation :: Float }

