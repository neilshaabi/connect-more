{-# LANGUAGE RecordWildCards #-}

module Board where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.List (isSubsequenceOf)

-- | Position in a board represented as a (column, row) pair
type Point = (Int,Int)

-- | Represention of the board to which @Piece@s are added
data Board = Board { pieces  :: Map Point Piece, -- ^ @Map@ storing the @Piece@ (value) at each @Point@ (key)
                     heights :: Map Int Int,     -- ^ @Map@ storing the height/number of pieces (value) in each column (key)
                     cols    :: Int,             -- ^ Number of columns
                     rows    :: Int }            -- ^ Number of rows

-- | Representation of pieces to be added to board (also used to represent players)
data Piece = None | Red | Blue
    deriving (Show, Eq, Ord)

-- | Empty board (no pieces placed)
emptyBoard :: Int -> Int -> Board
emptyBoard cols rows = Board {
    pieces  = Map.fromList [((x,y), None) | x <- [1..cols], y <- [1..rows]],
    heights = Map.fromList [(col,0) | col <- [1..cols]],
    cols = cols,
    rows = rows
    }

-- | Initial score (all individual scores set to 0)
initialScore :: Map Piece Int
initialScore = Map.fromList [(piece, 0) | piece <- [None, Red, Blue]]

-- | Gets the @Piece@ at a given @Point@ in a given @Board@
getPiece :: Board -> Point -> Piece
getPiece board coords = pieces board ! coords

-- | Gets the opposite @Piece@
opposite :: Piece -> Piece
opposite Red  = Blue
opposite Blue = Red
opposite None = None

-- | Updates a given @Board@ after adding a @Piece@ to a given @Point@
updateBoard :: Board -> (Int,Int) -> Piece -> Board
updateBoard Board{..} (col,row) player = Board {
    pieces  = Map.insert (col,row) player pieces,
    heights = Map.insert col row heights,
    cols = cols,
    rows = rows
    }

-- | Returns if a given @Board@ is full by checking if there are no unfilled columns
boardIsFull :: Board -> Bool
boardIsFull Board{..} = null $ Map.filter (< rows) heights

-- | Returns if a given @Piece@ has won by checking if their occupied positions 
--   include any winning combinations
hasWon :: Board -> Int -> Piece -> Bool
hasWon board@Board{..} win player = any (`isSubsequenceOf` occupied board player) allWins
    where
        -- All winning combinations based on size of board and number of pieces in a row to win
        allWins =
            let
                -- Possible winning combinations for each direction
                horizontalWins = [ [(c + z, r)     | z <- [0..(win - 1)]] | c <- [1..(cols - (win - 1))], r <- [1..rows] ]
                verticalWins   = [ [(c, r + z)     | z <- [0..(win - 1)]] | c <- [1..cols], r <- [1..(rows - (win - 1))] ]
                leftDiagWins   = [ [(c + z, r - z) | z <- [0..(win - 1)]] | c <- [1..(cols - (win - 1))], r <- [win..rows] ]
                rightDiagWins  = [ [(c + z, r + z) | z <- [0..(win - 1)]] | c <- [1..(cols - (win - 1))], r <- [1..(rows - (win - 1))] ]
            in
                -- Combine winning combinations for each direction into single list
                verticalWins ++ horizontalWins ++ leftDiagWins ++ rightDiagWins

-- | Returns list of @Point@s occupied by a given @Piece@ in a given @Board@
occupied :: Board -> Piece -> [Point]
occupied board player = Map.keys $ Map.filter (== player) (pieces board)
