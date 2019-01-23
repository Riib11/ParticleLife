module Display
( window
, background_color
, rate
, render
) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

import Debug
import Utility
import Particle
import Environment

{-/----------------------------------------------------------------------------/
 /  Display
/----------------------------------------------------------------------------/-}

window = InWindow "Particle Life" (800, 800) (10,10) :: Display

background_color = black :: Color

rate = 80 :: Int

{-/----------------------------------------------------------------------------/
 /  Render
/----------------------------------------------------------------------------/-}

render :: Environment -> IO Picture
render environment = let
  ps = environment_particles environment
  in return $ scale 5 5 $ Pictures $ map draw_particle ps

draw_particle :: P -> Picture
draw_particle p
  = p_translate p
  $ p_scale p
  $ p_color p
  $ circleSolid 1.0

p_translate :: Particle -> Picture -> Picture
p_translate p = let
  (x, y) = particle_position p
  in translate x y

p_scale :: Particle -> Picture -> Picture
p_scale p = scale 2.0 2.0

p_color :: Particle -> Picture -> Picture
p_color p = color $ int_to_color (particle_neighbors p)

int_to_color :: Int -> Color
int_to_color n = let
  x = (toFloat $ n + 1) / 5
  in makeColor x x x 1.0
