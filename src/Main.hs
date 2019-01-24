module Main (main) where

import System.Environment

import Graphics.Gloss.Interface.IO.Game

import Debug
import Utility

import Debug
import Utility

import Particle
import Environment
import Initialization
import Update

import Display
import Input

{-/----------------------------------------------------------------------------/
 /  Main
/----------------------------------------------------------------------------/-}

main = do
  args <- getArgs
  logged "start ParticleLife"
  playIO
    window
    background_color
    rate
    initial_conditions
    render
    input
    update
  
