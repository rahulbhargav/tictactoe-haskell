module Game where

import Data.Array

data Player = PlayerX | PlayerO deriving (Eq, Show)
data GameState = Running | GameOver (Player) | DrawMatch deriving (Eq, Show)
type Cell = Maybe Player
type Board = Array (Int, Int) Cell
data Game = Game {
    board :: Board,
    player :: Player,
    state :: GameState
} deriving (Eq, Show)

-- Constants
n ::Int 
n = 3

screenWidth :: Int 
screenWidth = 640

screenHeight :: Int 
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float 
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame = Game {
    board = array ((0,0), (2,2)) $ zip (range ((0,0), (2,2))) (repeat Nothing),
    player = PlayerO,
    state = Running
}