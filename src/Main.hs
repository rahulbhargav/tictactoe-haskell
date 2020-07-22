module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Array
import Data.Foldable ( asum )
import Data.Maybe

data Player = PlayerX | PlayerO deriving (Eq, Show)
data GameState = Running | GameOver (Player) | DrawMatch deriving (Eq, Show)
type Cell = Maybe Player
type Board = Array (Int, Int) Cell
data Game = Game {
    board :: Board,
    player :: Player,
    state :: GameState
} deriving (Eq, Show)

-- TODO: Modularize this file

-- Display Constant Values
n = 3
screenWidth = 640
screenHeight = 480
cellWidth = fromIntegral screenWidth / fromIntegral n
cellHeight = fromIntegral screenHeight / fromIntegral n

cellOColor = blue
cellXColor = red

playerColor :: Player -> Color
playerColor PlayerO = cellOColor
playerColor PlayerX = cellXColor

window = InWindow "Tic Tac Toe" (screenWidth, screenHeight) (200, 200)

main :: IO()
main = play window black 30 initialGame renderGame transformGame (const id)

isValidCoordinate = inRange ((0, 0), (n - 1, n - 1))

initialGame = Game {
    board = array ((0,0), (2,2)) $ zip (range ((0,0), (2,2))) (repeat Nothing),
    player = PlayerO,
    state = Running
}


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

renderXCells :: Board -> Picture
renderXCells b = 
    Pictures 
    $ map cellToPicture
    $ filter (\(_, c) -> c == (Just PlayerO))
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