module Main (main) where

import System.Environment

import Debug
import Game

{-/----------------------------------------------------------------------------/
 /  Main
/----------------------------------------------------------------------------/-}

main = do
  args <- getArgs               -- read command line arguments
  logged "start ParticleLife"   -- log start
  start                         -- start simulation
