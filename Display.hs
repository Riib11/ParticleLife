module Display
( window
, background_color
, rate
, render
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import Debug
import Particle
import Environment

{-/----------------------------------------------------------------------------/
 /  Display
/----------------------------------------------------------------------------/-}

window :: Display
window = InWindow "Particle Life" (600, 600) (10,10)

background_color :: Color
background_color = black

rate :: Int
rate = 80

{-/----------------------------------------------------------------------------/
 /  Render
/----------------------------------------------------------------------------/-}

render :: Environment -> Picture
render world = error "unimplemented"
