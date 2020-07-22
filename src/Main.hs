module Main where

import Graphics.Gloss

import Game
import Logic
import Rendering

window = InWindow "Tic Tac Toe" (screenWidth, screenHeight) (200, 200)

main :: IO()
main = play window bgColor 30 initialGame renderGame transformGame (const id)
