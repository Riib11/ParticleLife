module Game
( start
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

import Debug
import Particle
import Environment
import Display
import Input

{-/----------------------------------------------------------------------------/
 /  Play
/----------------------------------------------------------------------------/-}

start :: IO ()
start = play
  window
  background_color
  rate
  inital_conditions
  render
  input
  update

{-/----------------------------------------------------------------------------/
 /  Initial Conditions
/----------------------------------------------------------------------------/-}

inital_conditions :: Environment
inital_conditions = unimplemented

{-/----------------------------------------------------------------------------/
 /  Update
/----------------------------------------------------------------------------/-}

update :: Float -> Environment -> Environment
update dtime world = unimplemented
