module Config where

import Graphics.Gloss

import Board ( Piece(..) )

-- Size of game window in pixels
windowSize :: (Int,Int)
windowSize = (900,830)

-- Size of board in pixels
boardSize :: (Int,Int)
boardSize = (650,550)

-- Accessors for board dimensions in pixels
boardWidth, boardHeight :: Int
boardWidth  = fst boardSize
boardHeight = snd boardSize

-- Position window in centre of monitor (on 14-inch Macbook)
windowPos :: (Int,Int)
windowPos  = (310,200)

-- Scale piece radius appropriately as a function of the number of columns and rows
pieceRadius :: Int -> Int -> Float
pieceRadius cols rows = 210 / fromIntegral (max cols rows)

-- Calculate pixels occupied by each piece
xUnit, yUnit :: Int -> Float
xUnit cols = fromIntegral boardWidth  / fromIntegral cols
yUnit rows = fromIntegral boardHeight / fromIntegral rows

-- Grey colour used for text
grey :: Color
grey = greyN 0.4

-- Get the colour of each type of piece
getColour :: Piece -> Color
getColour None = makeColor 0.1 0.2 0.3 0.1
getColour Red  = makeColor 1 0.4 0.4 0.8
getColour Blue = makeColor 0.4 0.6 1 0.9

-- Get the background colour to display when a player wins
getBG :: Piece -> Color
getBG None = greyN 0.9
getBG Red  = makeColor 1 0.4 0.4 0.3
getBG Blue = makeColor 0.4 0.6 1 0.3