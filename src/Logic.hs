module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game

import Data.Array
import Data.Foldable ( asum )
import Data.Maybe

isValidCoordinate = inRange ((0, 0), (n - 1, n - 1))

switchPlayer :: Game -> Game
switchPlayer g =
    case (player g) of
        PlayerO -> g { player = PlayerX }
        PlayerX -> g { player = PlayerO }

markPlayerMove :: Game -> (Int, Int) -> Game
markPlayerMove g cellIdx 
    | (isValidCoordinate cellIdx) && (b ! cellIdx == Nothing) = g { board = b // [(cellIdx, Just p)] }
    | otherwise = g
    where b = board g
          p = player g

coordsToCellIdx :: (Float, Float) -> (Int, Int)
coordsToCellIdx (x, y) = 
    (floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth),
      floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight))

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    case state game of 
        Running -> 
            checkGameCompletion
            $ switchPlayer
            $ markPlayerMove game 
            $ coordsToCellIdx mousePos
        GameOver(_) -> initialGame
        DrawMatch -> initialGame
transformGame (EventKey (SpecialKey KeySpace) Up _ _) game = 
    case state game of 
        Running -> game { state = DrawMatch }
        GameOver(_) -> initialGame
        DrawMatch -> initialGame
transformGame _ game = game

full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _ = Nothing

findWinner :: Board -> Maybe Player
findWinner b = 
    asum 
    $ map full 
    $ rows ++ cols ++ diags
    where rows  = [[b ! (i,j) | i <- [0..n-1]] | j <- [0..n-1]]
          cols  = [[b ! (j,i) | i <- [0..n-1]] | j <- [0..n-1]]
          diags = [[b ! (i,i) | i <- [0..n-1]]
                  ,[b ! (i,j) | i <- [0..n-1], let j = n-1-i ]]
                
allCellsFilled :: Board -> Bool
allCellsFilled b = null $ filter (\(_, e) -> e == Nothing) $  assocs b 

checkGameCompletion :: Game -> Game
checkGameCompletion g = 
    case findWinner (board g) of 
        Just p -> g { state = GameOver (p) }
        Nothing ->  if allCellsFilled $ board g then g { state = DrawMatch } else g

