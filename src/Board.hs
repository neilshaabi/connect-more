{-# LANGUAGE RecordWildCards #-}

module Board where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.List (isSubsequenceOf)

-- Position in a board represented as a (column, row) pair
type Point = (Int,Int)

-- Represention of the board to which pieces are added
data Board = Board { pieces  :: Map Point Piece, -- Map storing the piece (value) at each position (key)
                     heights :: Map Int Int,     -- Map storing the height/number of pieces (value) in each column (key)
                     cols    :: Int,             -- Number of columns
                     rows    :: Int }            -- Number of grids

-- Representation of pieces to be added to board, also used to represent players
data Piece = None | Red | Blue
    deriving (Show, Eq, Ord)

-- Empty board with no pieces placed
emptyBoard :: Int -> Int -> Board
emptyBoard cols rows = Board {
    pieces  = Map.fromList [((x,y), None) | x <- [1..cols], y <- [1..rows]],
    heights = Map.fromList [(col,0) | col <- [1..cols]],
    cols = cols,
    rows = rows
    }

-- Initial score - all individual scores set to 0
initialScore :: Map Piece Int
initialScore = Map.fromList [(piece, 0) | piece <- [None, Red, Blue]]

-- Get the piece at a given position in the board
getPiece :: Board -> Point -> Piece
getPiece board coords = pieces board ! coords

-- Get the opposite piece/player 
opposite :: Piece -> Piece
opposite Red  = Blue
opposite Blue = Red
opposite None = None

-- Update the board after adding a piece at a given position
updateBoard :: Board -> (Int,Int) -> Piece -> Board
updateBoard Board{..} (col,row) player = Board {
    pieces  = Map.insert (col,row) player pieces,
    heights = Map.insert col row heights,
    cols = cols,
    rows = rows
    }

-- Return if the board is full by checking if there are no unfilled columns
boardIsFull :: Board -> Bool
boardIsFull Board{..} = null $ Map.filter (< rows) heights

-- Return if a player has won by checking if their occupied positions include any winning combinations
hasWon :: Board -> Int -> Piece -> Bool
hasWon board@Board{..} win player = any (`isSubsequenceOf` occupied board player) allWins
    where
        -- All winning combinations based on size of board and number of pieces in a row to win
        allWins =
            let
                -- Get possible winning combinations for each direction
                horizontalWins = [ [(c + z, r)     | z <- [0..(win - 1)]] | c <- [1..(cols - (win - 1))], r <- [1..rows] ]
                verticalWins   = [ [(c, r + z)     | z <- [0..(win - 1)]] | c <- [1..cols], r <- [1..(rows - (win - 1))] ]
                leftDiagWins   = [ [(c + z, r - z) | z <- [0..(win - 1)]] | c <- [1..(cols - (win - 1))], r <- [win..rows] ]
                rightDiagWins  = [ [(c + z, r + z) | z <- [0..(win - 1)]] | c <- [1..(cols - (win - 1))], r <- [1..(rows - (win - 1))] ]
            in
                -- Combine winning combinations for each direction into single list
                verticalWins ++ horizontalWins ++ leftDiagWins ++ rightDiagWins

-- Get list of positions occupied in the board by a given player
occupied :: Board -> Piece -> [Point]
occupied board player = Map.keys $ Map.filter (== player) (pieces board)