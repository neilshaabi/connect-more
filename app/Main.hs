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

-- | Data type defining different game states/screens
data State = Title | SetCols | SetRows | SetWin | Help | Play | Endgame
    deriving (Eq)

-- | Data type storing information about the game
data World = World {
    state     :: State,          -- ^ Current @State@ of game
    prevState :: State,          -- ^ Previous @State@ (recorded to return from help menu)
    win       :: Int,            -- ^ Number of pieces in a row required to win
    board     :: Board,          -- ^ Current @Board@
    moves     :: [Point],        -- ^ Record of moves made in order in the current game
    player    :: Piece,          -- ^ Current player
    starter   :: Piece,          -- ^ Starting player next game (alternates every game)
    winner    :: Piece,          -- ^ Winning player
    score     :: Map Piece Int   -- ^ Record of scores (players are keys, games won are values)
}

-- | Initial @World@ rendered when a new game starts
initialWorld :: World
initialWorld = World {
    state     = Title,           -- ^ Start on the title screen
    prevState = Title,           -- ^ Set previous state to title
    win       = 4,               -- ^ Default number of pieces in a row to win
    board     = emptyBoard 7 6,  -- ^ Reset board with default columns and rows
    moves     = [],              -- ^ No moves made
    player    = Red,             -- ^ Red starts first game by default
    starter   = Blue,            -- ^ Blue starts next game
    winner    = None,            -- ^ No winner yet
    score     = initialScore     -- ^ All scores set to 0
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


-- | Renders a @World@ as a @Picture@ based on the state of the game
render :: World -> Picture
render World{..} = 
    let
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


-- | Updates the @World@ based on user interactions (key presses)
handleInput :: Event -> World -> World

-- Handles spacebar presses - starts new game
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@World{..} = case state of
    Title   -> world { state    = Play }                  -- Start new game
    Endgame -> world { state    = Play,                   -- Start new game
                        board   = emptyBoard (cols board) (rows board), -- Reset board
                        moves   = [],                     -- Reset move history
                        player  = starter,                -- Alternate starting player
                        starter = opposite starter,       -- Opposite player starts next game
                        winner  = None                    -- Reset winner
                        }
    Help    -> world                                     -- Ignore if on help menu
    Play    -> world                                     -- Ignore if playing game
    _       -> world { state = Play }                    -- Start game from settings menu

-- Handles down arrow key presses - navigate settings menu
handleInput (EventKey (SpecialKey KeyDown) Down _ _) world@World{..} = case state of
    SetCols  -> world { state = SetRows }  -- Move down settings menu to set number of rows
    SetRows  -> world { state = SetWin }   -- Move down settings menu to set number of pieces to win
    _        -> world                      -- Ignore if not on settings screen

-- Handles up arrow key presses - navigate settings menu
handleInput (EventKey (SpecialKey KeyUp) Down _ _) world@World{..} = case state of
    SetRows -> world { state = SetCols }   -- Move up settings menu to number of columns
    SetWin  -> world { state = SetRows }   -- Move up settings menu to number of rows
    _       -> world                       -- Ignore if not on settings screen

-- Handle left arrow key presses - undo previous move
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) world@World{..} = case state of
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

-- Handles character key presses
handleInput (EventKey (Char c) Down _ _) world@World{..} = 
    let 
        cs = cols board
        rs = rows board
    in 
        case state of
        Title   | c == 's'                                 -> world { state = SetCols }                     -- Go to settings menu
        Play    | c `elem` ['1'..(intToDigit cs)]          -> makeMove world (digitToInt c)                 -- Add piece to column entered
        SetCols | c `elem` ['3'..'9']                      -> world { state = SetRows,                      -- Move down to next setting
                                                                      board = emptyBoard (digitToInt c) rs, -- Set new number of columns
                                                                      win   = getWin win (digitToInt c) rs  -- Update pieces to win if necessary
                                                                    }
        SetRows | c `elem` ['3'..'9']                      -> world { state = SetWin,                       -- Move down to next option
                                                                      board = emptyBoard cs (digitToInt c), -- Set new number of rows
                                                                      win   = getWin win cs (digitToInt c)  -- Update pieces to win if necessary
                                                                    }
        SetWin  | c `elem` ['3'..(intToDigit $ min cs rs)] -> world { win   = digitToInt c }                -- Set new number pieces to win
        _       | c == 'h', state /= Help                  -> world { state = Help, prevState = state }     -- Show help menu
        _       | c == 'h'                                 -> world { state = prevState }                   -- Return to previous screen from help menu
        _       | c == 'q'                                 -> initialWorld                                  -- Quit to title screen
        _                                                  -> world                                         -- Ignore other character key presses

-- Ignore any other inputs
handleInput _ world = world

-- | Attempts to add a @Piece@ to the @Board@ stored in the @World@neilshaabi
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
    | boardIsFull board       = world { state  = Endgame,
                                        score  = Map.adjust (+ 1) None score }
    -- End the game if a player has won
    | hasWon board win player = world { state  = Endgame,
                                        winner = player,
                                        score  = Map.adjust (+ 1) player score }
    -- Continue playing the game, updating the next player
    | otherwise               = world { state  = Play,
                                        player = opposite player }

-- Do nothing unless called after making a move
update _ world = world


-- | Returns the @Board@ after removing the @Piece@ at a given @Point@
undoMove :: Board -> Point -> Board
undoMove board@Board{..} coords = board {
    pieces  = Map.insert coords None pieces,            -- Remove @Piece@ at @Point@
    heights = Map.adjust (+ (-1)) (fst coords) heights  -- Decrement height of column
    }

-- | Restricts the maximum number of pieces to win to the smallest @Board@ dimension
getWin :: Int -> Int -> Int -> Int
getWin currWin cols rows = if currWin > maxWin then maxWin else currWin
    where maxWin = min cols rows
