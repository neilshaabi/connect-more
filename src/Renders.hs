{-# LANGUAGE RecordWildCards #-}

module Renders where

import Graphics.Gloss
import Data.Map (Map, (!))

import Config
import Board

-- | Helper function for rendering text
--   Source: https://github.com/dixonary/hake
text' :: Color -> Float -> Float -> String -> Picture
text' col size boldness str =
    color col $
    pictures [translate x y $ scale (size / 100) (size / 100) $ text str
         | x <- [(-boldness),(0.5-boldness)..boldness],
           y <- [(-boldness),(0.5-boldness)..boldness] ]

-- | x-coordinate for left-aligning text in setup/help menus
alignL :: Float
alignL = -360

-- | Renders title screen with logo and text
renderTitle :: [Picture]
renderTitle = [
    translate (-35) 100 $ color (getColour Red) $ circleSolid 50,
    translate 35 100  $ color (getColour Blue) $ circleSolid 50,
    translate (-175) (-20)  $ text' grey 40 1   "connect more",
    translate (-223) (-80) $ text' grey 25 0.8 "Press space to start game",
    translate (-160) (-130) $ text' grey 20 0.8 "Press s to view settings",
    translate (-140) (-180) $ text' grey 20 0.8 "Press h to view help"
    ]

-- | Renders settings menu with board dimensions, pieces in a row to win and instructions
renderSettings :: Board -> Int -> Int -> Float -> [Picture]
renderSettings Board{..} win maxWin rectY = [
    translate 0 rectY $ color (greyN 0.95) $ rectangleSolid 800 60, -- Rectangle indicating selected field
    translate alignL 290 $ text' grey 40 1.2 "Game settings",
    translate alignL 210 $ text' grey 25 1 "Enter number within range to change",
    translate alignL 140 $ text' grey 25 1 ("- Number of columns: " ++ show cols ++ "    (3-9)"),
    translate alignL 80  $ text' grey 25 1 ("- Number of rows: " ++ show rows ++ "      (3-9)"),
    translate alignL 20  $ text' grey 25 1 ("- Pieces in a row to win: " ++ show win  ++ " (3-" ++ show maxWin ++ ")"),
    translate alignL (-100) $ text' grey 25 1 "Navigate with up/down arrow keys",
    translate alignL (-160) $ text' grey 25 1 "Press space to start game"
    ]

-- | Renders help menu with gameplay instructions and controls
renderHelp :: [Picture]
renderHelp = [
    translate alignL 290    $ text' grey 40 1.2 "Help menu",
    translate alignL 210    $ text' grey 25 1.2 "Gameplay",
    translate alignL 150    $ text' grey 20 0.8 "- Players take turns adding one piece to an unfilled",
    translate alignL 100    $ text' grey 20 0.8 "  column, until one player wins by placing x pieces",
    translate alignL 50     $ text' grey 20 0.8 "  in a row (x specified by user during setup).",
    translate alignL 0      $ text' grey 20 0.8 "- Draw is declared when the board is filled.",
    translate alignL (-70)  $ text' grey 25 1.2 "Controls (keyboard)",
    translate alignL (-130) $ text' grey 20 0.8 "- Numbers 1-7: add pieces to column",
    translate alignL (-180) $ text' grey 20 0.8 "- Left arrow key: undo previous move",
    translate alignL (-230) $ text' grey 20 0.8 "- q: quit to main menu, reset score",
    translate alignL (-280) $ text' grey 20 0.8 "- h: show/hide help menu"
    ]

-- | Renders ongoing game with score, pieces to win, board, column numbers and current player
renderPlay :: Map Piece Int -> Int -> Board -> Piece -> [Picture]
renderPlay score win board player = [
    renderScore score,
    renderGoal win,
    renderBoard board,
    renderColNums board,
    translate (-150) (-370) $ text' grey 22 0.8 "Current player: ",
    translate 72 (-370) $ text' (getColour player) 22 0.8 $ show player
    ]

-- | Renders current score for each player, including number of draws
renderScore :: Map Piece Int -> Picture
renderScore score = translate (-184) 350 $ text' grey 20 0.8 (
    "Draw: " ++ show (score ! None) ++ " | " ++
    "Red: "  ++ show (score ! Red)  ++ " | " ++
    "Blue: " ++ show (score ! Blue)
    )

-- | Renders current numbers of pieces in a row to win, with help instructions
renderGoal :: Int -> Picture
renderGoal win = translate (-218) 300 $ text' grey 22 0.8 (
    "Connect " ++ show win ++ " (press h for help)")

-- | Renders board in the centre of the game window
--   Adapted from: https://github.com/dixonary/hake
renderBoard :: Board -> Picture
renderBoard board@Board{..} = pictures $ [renderPiece col row | col <- [1..cols], row <- [1..rows]]
    where
        renderPiece col row =
            let
                -- Midpoint of piece relative to board dimensions
                xRel = fromIntegral (col - 1) - (fromIntegral cols - 1) / 2
                yRel = fromIntegral (row - 1) - (fromIntegral rows - 1) / 2

                -- Position based on distance from middle
                xPos = xRel * xUnit cols
                yPos = yRel * yUnit rows
            in
                -- Display piece as circle in correct colour
                translate xPos yPos $ color (getColour piece) $ circleSolid (pieceRadius cols rows)
                    where
                         -- Get the piece at the current position
                        piece = getPiece board (col,row)

-- | Renders column numbers below each column for player's reference
renderColNums :: Board -> Picture
renderColNums Board{..} = pictures $ [renderNumber col | col <- [1..cols]]
    where
        renderNumber col =
            let
                xRel = fromIntegral (col - 1) - (fromIntegral cols - 1) / 2 -- Midpoint of number relative to number of columns
                xPos = xRel * xUnit cols - 11                               -- Position based on distance from middle
            in 
                translate xPos (-320) $ text' grey 30 1 (show col)          -- Display column number

-- | Renders endgame screen with score, board, winning player and instructions
renderEndgame :: Map Piece Int -> Int -> Board -> Piece -> [Picture]
renderEndgame score goal board winner = [
    translate 0 0 $ color (getBG winner) $ rectangleSolid 1000 1000, -- Change background colour depending on winner
    renderGoal goal,
    renderScore score,
    renderBoard board,
    renderWinner winner,
    translate (-163) (-370) $ text' grey 22 0.8 "Press space to restart"
    ]

-- | Renders text indicating winning player or draw
renderWinner :: Piece -> Picture
renderWinner None   = translate (-50) (-320) $ text' grey 30 1 "Draw!"
renderWinner player = translate (-90) (-320) $ text' (getColour player) 30 1 (show player ++ " wins!")
