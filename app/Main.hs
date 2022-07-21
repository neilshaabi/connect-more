{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.IO.Game hiding (Point)
import Data.Char (intToDigit, digitToInt)
import Data.Map (Map, (!))

import qualified Data.Map as Map

import Board
import Config
import Renders

-- | Data type defining different game screens
data Screen = Title | SetCols | SetRows | SetWin | Help | Play | Endgame
    deriving (Eq)

-- | Data type storing information about the game
data World = World 
    { screen     :: Screen         -- ^ Current @Screen@ in game
    , prevScreen :: Screen         -- ^ Previous @Screen@ (recorded to return from help menu)
    , win        :: Int            -- ^ Number of pieces in a row required to win
    , board      :: Board          -- ^ Current @Board@
    , moves      :: [Point]        -- ^ Record of moves made in order in the current game
    , player     :: Piece          -- ^ Current player
    , starter    :: Piece          -- ^ Starting player next game (alternates every game)
    , winner     :: Piece          -- ^ Winning player
    , score      :: Map Piece Int  -- ^ Record of scores (players are keys, games won are values)
    }

-- | Initial @World@ rendered when a new game starts
initialWorld :: World
initialWorld = World 
    { screen     = Title
    , prevScreen = Title
    , win        = 4
    , board      = emptyBoard 7 6
    , moves      = []
    , player     = Red
    , starter    = Blue
    , winner     = None
    , score      = initialScore
    }


-- | Main entry point to program, handles gameplay
main :: IO ()
main = play
    (InWindow "Connect More" windowSize windowPos) -- Display mode
    white                                          -- Background colour
    60                                             -- Updates per second of real time
    initialWorld                                   -- Initial world
    render                                         -- Rendering function
    handleInput                                    -- Input handler
    update                                         -- Update handler


-- | Renders a @World@ as a @Picture@ based on the game screen
render :: World -> Picture
render World{..} = 
    let maxWin = min (cols board) (rows board)
    in pictures $ case screen of
        Title   -> renderTitle
        SetCols -> renderSettings board win maxWin 152
        SetRows -> renderSettings board win maxWin 92
        SetWin  -> renderSettings board win maxWin 32
        Help    -> renderHelp
        Play    -> renderPlay score win board player
        Endgame -> renderEndgame score win board winner


-- | Updates the @World@ based on user interactions (key presses)
handleInput :: Event -> World -> World

-- Handles spacebar presses - starts new game
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@World{..} = case screen of
    Title   -> world { screen  = Play }
    Endgame -> world { screen  = Play
                     , board   = emptyBoard (cols board) (rows board)
                     , moves   = []
                     , player  = starter
                     , starter = opposite starter
                     , winner  = None
                     }
    Help    -> world
    Play    -> world
    _       -> world { screen = Play }

-- Handles down arrow key presses - navigate settings menu
handleInput (EventKey (SpecialKey KeyDown) Down _ _) world@World{..} = case screen of
    SetCols  -> world { screen = SetRows }
    SetRows  -> world { screen = SetWin }
    _        -> world

-- Handles up arrow key presses - navigate settings menu
handleInput (EventKey (SpecialKey KeyUp) Down _ _) world@World{..} = case screen of
    SetRows -> world { screen = SetCols }
    SetWin  -> world { screen = SetRows }
    _       -> world

-- Handle left arrow key presses - undo previous move
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) world@World{..} = case screen of
    Play | not $ null moves -> world { board  = undoMove board (last moves)
                                     , moves  = init moves
                                     , player = opposite player
                                     }
    Endgame                 -> world { screen = Play
                                     , board  = undoMove board (last moves)
                                     , moves  = init moves
                                     , winner = None
                                     , score  = Map.adjust (+ (-1)) winner score
                                     }
    _                       -> world

-- Handles character key presses
handleInput (EventKey (Char c) Down _ _) world@World{..} = 
    let 
        cs = cols board
        rs = rows board
    in 
        case screen of
        Title   | c == 's'                                 -> world { screen = SetCols }
        Play    | c `elem` ['1'..(intToDigit cs)]          -> makeMove world (digitToInt c)
        SetCols | c `elem` ['3'..'9']                      -> world { screen = SetRows
                                                                    , board  = emptyBoard (digitToInt c) rs
                                                                    , win    = getWin win (digitToInt c) rs
                                                                    }
        SetRows | c `elem` ['3'..'9']                      -> world { screen = SetWin
                                                                    , board  = emptyBoard cs (digitToInt c)
                                                                    , win    = getWin win cs (digitToInt c)
                                                                    }
        SetWin  | c `elem` ['3'..(intToDigit $ min cs rs)] -> world { win    = digitToInt c }
        _       | c == 'h', screen /= Help                 -> world { screen = Help, prevScreen = screen }
        _       | c == 'h'                                 -> world { screen = prevScreen }
        _       | c == 'q'                                 -> initialWorld
        _                                                  -> world

-- Ignore any other inputs
handleInput _ world = world

-- | Attempts to add a @Piece@ to the @Board@ stored in the @World@
--   at the given column number
makeMove :: World -> Int -> World
makeMove world@World{..} col =
    let
        row = heights board ! col + 1                 -- Row to add piece to (column height + 1)
        newBoard = updateBoard board (col,row) player -- Add piece to board in given column
    in
        -- Update game if column is not full, otherwise do not make move
        if row <= rows board 
        then update 0 world { board = newBoard, moves = moves ++ [(col,row)] }
        else world


-- | Updates the @World@ when a move is made
update :: Float -> World -> World
update 0 world@World{..}

    -- Declare draw if the @Board@ is full (no unfilled columns)
    | boardIsFull board       = world { screen  = Endgame,
                                        score  = Map.adjust (+ 1) None score 
                                      }
    -- End the game if a player has won
    | hasWon board win player = world { screen  = Endgame
                                      , winner = player
                                      , score  = Map.adjust (+ 1) player score 
                                      }
    -- Continue playing the game, updating the next player
    | otherwise               = world { screen  = Play
                                      , player = opposite player
                                      }

-- Do nothing unless called after making a move
update _ world = world


-- | Returns the @Board@ after removing the @Piece@ at a given @Point@
undoMove :: Board -> Point -> Board
undoMove board@Board{..} coords = board 
    { pieces  = Map.insert coords None pieces             -- Remove @Piece@ at @Point@
    , heights = Map.adjust (+ (-1)) (fst coords) heights  -- Decrement height of column
    }

-- | Restricts the maximum number of pieces to win to the smallest @Board@ dimension
getWin :: Int -> Int -> Int -> Int
getWin currWin cols rows = min currWin (min cols rows)
