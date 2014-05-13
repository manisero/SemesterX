module Main(main) where

import Logic.Game_WolfNSheep
import Presentation.GameLoop

main :: IO ()
main = startGame emptyBoard Wolf

emptyBoard :: Board
emptyBoard = Board 8 (-1, -1) [ (0, 1), (0, 3), (0, 5), (0, 7) ]
