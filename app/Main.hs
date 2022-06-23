{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.IO.Game hiding (Point)
import Data.Char (intToDigit, digitToInt)
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Config
import Board
import Renders

-- Data type defining different game states/screens
data State = Title | SetCols | SetRows | SetWin | Help | Play | Endgame
    deriving (Eq)

-- Data type storing game information
data World = World {
    state     :: State,          -- Current state of game
    prevState :: State,          -- Previous state (recorded to return from help menu)
    win       :: Int,            -- Number of pieces in a row to win
    board     :: Board,          -- Current board
    moves     :: [Point],        -- Record of moves made in order
    player    :: Piece,          -- Current player
    starter   :: Piece,          -- Starter of next game (alternates every game)
    winner    :: Piece,          -- Winning player
    score     :: Map Piece Int   -- Record of scores (players are keys, games won are values)
}

-- Initial World structure rendered when game starts
initialWorld :: World
initialWorld = World {
    state     = Title,           -- Start on the title screen
    prevState = Title,           -- Set previous state to title
    win       = 4,               -- Default number of pieces in a row to win
    board     = emptyBoard 7 6,  -- Reset board with default columns and rows
    moves     = [],              -- No moves made
    player    = Red,             -- Red starts first game
    starter   = Blue,            -- Blue starts next game
    winner    = None,            -- No winner yet
    score     = initialScore     -- All scores set to 0
}


-- Main entry point to program, handles gameplay
main :: IO ()
main = play
    (InWindow "Connect More" windowSize windowPos) -- Display mode
    white                                          -- Background colour
    60                                             -- Updates per second of real time
    initialWorld                                   -- Initial world
    render                                         -- Rendering function
    handleInput                                    -- Input handler
    update                                         -- Update handler


-- Render world to picture based on state of game
render :: World -> Picture
render World{..} = 
    let 
        -- Maximum number of pieces in a row to win limited by smallest board dimension
        maxWin = min (cols board) (rows board)
    in 
        pictures $ case state of
        Title   -> renderTitle
        SetCols -> renderSettings board win maxWin 152
        SetRows -> renderSettings board win maxWin 92
        SetWin  -> renderSettings board win maxWin 32
        Help    -> renderHelp
        Play    -> renderPlay score win board player
        Endgame -> renderEndgame score win board winner


-- Updates world based on user's interactions with game through key presses
handleInput :: Event -> World -> World

-- Handle spacebar press - start new game
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@World{..} = 
    case state of
        Title   -> world { state   = Play }                  -- Start new game
        Endgame -> world { state   = Play,                   -- Start new game
                           board   = emptyBoard (cols board) (rows board), -- Reset board, maintaining size
                           moves   = [],                     -- Reset move history
                           player  = starter,                -- Alternate starting player
                           starter = opposite starter,       -- Opposite player starts next game
                           winner  = None                    -- Reset winner
                         }
        Help    -> world                                     -- Ignore if on help menu
        Play    -> world                                     -- Ignore if playing game
        _       -> world { state = Play }                    -- Start game from settings menu

-- Handle down arrow key press - navigate settings menu
handleInput (EventKey (SpecialKey KeyDown) Down _ _) world@World{..} = 
    case state of
        SetCols  -> world { state = SetRows }  -- Move down settings menu to set number of rows
        SetRows  -> world { state = SetWin }   -- Move down settings menu to set number of pieces in a row to win
        _        -> world                      -- Ignore if not on settings screen

-- Handle up arrow key press - navigate settings menu
handleInput (EventKey (SpecialKey KeyUp) Down _ _) world@World{..} =
    case state of
        SetRows -> world { state = SetCols }   -- Move up to settings menu to number of columns
        SetWin  -> world { state = SetRows }   -- Move up to settings menu to number of rows
        _       -> world                       -- Ignore if not on settings screen

-- Handle left arrow key press - undo previous move
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) world@World{..} =
    case state of
        Play | not $ null moves -> world { board  = undoMove board (last moves),      -- Remove last piece added until empty
                                           moves  = init moves,                       -- Remove record of last move
                                           player = opposite player                   -- Go back to previous player
                                         }
        Endgame                 -> world { state  = Play,                             -- Resume previous game
                                           board  = undoMove board (last moves),      -- Remove last piece added
                                           moves  = init moves,                       -- Remove record of last move
                                           winner = None,                             -- Undeclare winner
                                           score  = Map.adjust (+ (-1)) winner score  -- Decrement winner's score
                                         }
        _                       -> world                                              -- Ignore in other states

-- Handle character key presses
handleInput (EventKey (Char c) Down _ _) world@World{..} = 
    let cs = cols board -- Shorthand for number of columns
        rs = rows board -- Shorthand for number of rows
    in case state of
        Title   | c == 's'                                 -> world { state = SetCols }                     -- Go to settings menu
        Play    | c `elem` ['1'..(intToDigit cs)]          -> makeMove world (digitToInt c)                 -- Add piece to column entered
        SetCols | c `elem` ['3'..'9']                      -> world { state = SetRows,                      -- Move down to next setting
                                                                      board = emptyBoard (digitToInt c) rs, -- Set new number of columns
                                                                      win   = win' win (digitToInt c) rs    -- Update pieces to win if necessary
                                                                    }
        SetRows | c `elem` ['3'..'9']                      -> world { state = SetWin,                       -- Move down to next setting
                                                                      board = emptyBoard cs (digitToInt c), -- Set new number of rows
                                                                      win   = win' win cs (digitToInt c)    -- Update pieces to win if necessary
                                                                    }
        SetWin  | c `elem` ['3'..(intToDigit $ min cs rs)] -> world { win   = digitToInt c }                -- Set new number of rows
        _       | c == 'h', state /= Help                  -> world { state = Help, prevState = state }     -- Show help menu
        _       | c == 'h'                                 -> world { state = prevState }                   -- Return to previous screen from help menu
        _       | c == 'q'                                 -> initialWorld                                  -- Quit to title screen
        _                                                  -> world                                         -- Ignore other character key presses

-- Ignore any other inputs
handleInput _ world = world

-- Attempt to add a piece to the column entered
makeMove :: World -> Int -> World
makeMove world@World{..} col =
    let
        row = heights board ! col + 1                 -- Get row to add piece to (column height + 1)
        newBoard = updateBoard board (col,row) player -- Add piece to board in selected column
    in
        -- Update game if column to add piece to is not full, otherwise do not amke move
        if row <= rows board then
            update 0 world { board = newBoard, moves = moves ++ [(col,row)] }
        else world


-- Update world when player makes a move
update :: Float -> World -> World
update 0 world@World{..}

    -- Declare draw if board is full (no unfilled columns)
    | boardIsFull board       = world { state  = Endgame,
                                        score  = Map.adjust (+ 1) None score }
    -- End game if player has won
    | hasWon board win player = world { state  = Endgame,
                                        winner = player,
                                        score  = Map.adjust (+ 1) player score }
    -- Continue playing game, set next player
    | otherwise               = world { state  = Play,
                                        player = opposite player }

-- Do nothing unless called after making a move
update _ world = world


-- Return the board after removing the piece at a given position
undoMove :: Board -> Point -> Board
undoMove board@Board{..} coords = board {
    pieces  = Map.insert coords None pieces,            -- Remove piece at position (add None)
    heights = Map.adjust (+ (-1)) (fst coords) heights  -- Decrement height of column
    }

-- Limit the maximum number of pieces in a row to win to the smallest board dimension
win' :: Int -> Int -> Int -> Int
win' currWin cols rows = if currWin > maxWin then maxWin else currWin
    where maxWin = min cols rows