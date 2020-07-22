module Rendering where

import Game

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Array

cellOColor = blue
cellXColor = red
bgColor = black

playerColor :: Player -> Color
playerColor PlayerO = cellOColor
playerColor PlayerX = cellXColor

cellX :: Picture
cellX = Pictures[ rotate 45.0 
                    $ Color cellXColor 
                    $ rectangleSolid side 10.0
                , rotate (-45.0) 
                    $ Color cellXColor 
                    $ rectangleSolid side 10.0
                ] 
        where side = min cellWidth cellHeight * 0.75

cellO :: Picture
cellO = Color cellOColor (ThickCircle ((min cellWidth cellHeight) * 0.375) 10)

boardGrid :: Picture
boardGrid = 
    Pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0, i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

posPictureInCell :: (Int, Int) -> Picture -> Picture 
posPictureInCell (row, col) pic = 
    translate x y pic
    where x = fromIntegral row * cellWidth + cellWidth * 0.5
          y = fromIntegral col * cellHeight + cellHeight * 0.5


cellToPicture:: ((Int, Int), Cell) -> Picture
cellToPicture (idx, c) = 
    posPictureInCell idx
    $ toPicture c
    where toPicture (Just PlayerX) = cellX
          toPicture (Just PlayerO) = cellO
          toPicture Nothing = blank

renderCells :: Board -> Player -> Picture
renderCells b p = 
    Pictures 
    $ map cellToPicture
    $ filter (\(_, c) -> c == (Just p))
    $ assocs b

transformMessagePicture :: Picture -> Picture
transformMessagePicture p = 
    translate
        (fromIntegral screenWidth * (-0.25))
        (fromIntegral screenHeight * (0.1))
        (Scale 0.5 0.5 p)

renderGame :: Game -> Picture
renderGame game = 
    case state game of 
        Running -> translate
            (fromIntegral screenWidth * (-0.5))
            (fromIntegral screenHeight * (-0.5))
            (Pictures [
                renderCells (board game) PlayerO, 
                renderCells (board game) PlayerX, 
                (color green boardGrid) 
            ])
        GameOver(p) -> transformMessagePicture $ Color (playerColor p) (Text $ show(p) ++ " won!")
        DrawMatch -> transformMessagePicture $ Color white ((Text "Draw Match"))
