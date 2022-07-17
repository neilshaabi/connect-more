module Config where

import Graphics.Gloss ( Color, greyN, makeColor )

import Board ( Piece(..) )

-- | Size of the game window in pixels
windowSize :: (Int,Int)
windowSize = (900,830)

-- | Size of the @Board@ in pixels
boardSize :: (Int,Int)
boardSize = (650,550)

-- | Accessors for the @Board@'s dimensions in pixels
boardWidth, boardHeight :: Int
boardWidth  = fst boardSize
boardHeight = snd boardSize

-- | Positions the game window in the centre of the monitor (on 14-inch Macbook)
windowPos :: (Int,Int)
windowPos  = (310,200)

-- | Calculates the appropriate @Piece@ radius as a function of the 
--   number of columns and rows in the @Board@
pieceRadius :: Int -> Int -> Float
pieceRadius cols rows = 210 / fromIntegral (max cols rows)

-- | Calculates the number of pixels occupied by each piece
xUnit, yUnit :: Int -> Float
xUnit cols = fromIntegral boardWidth  / fromIntegral cols
yUnit rows = fromIntegral boardHeight / fromIntegral rows

-- | Grey @Color@ used for text
grey :: Color
grey = greyN 0.4

-- | Gets the @Color@ associated with a given @Piece@
getColour :: Piece -> Color
getColour None = makeColor 0.1 0.2 0.3 0.1
getColour Red  = makeColor 1 0.4 0.4 0.8
getColour Blue = makeColor 0.4 0.6 1 0.9

-- | Gets the background @Color@ to display when a given @Piece@ wins
getBG :: Piece -> Color
getBG None = greyN 0.9
getBG Red  = makeColor 1 0.4 0.4 0.3
getBG Blue = makeColor 0.4 0.6 1 0.3
