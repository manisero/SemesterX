module Main(main) where

import Logic.Game
import Presentation.GameLoop

main :: IO ()
main = startGame emptyBoard Wolf

emptyBoard :: Board
emptyBoard = Board
				8                                  -- size (8 x 8)
				(-1, -1)                           -- Wolf position (not initialized)
				[ (0, 1), (0, 3), (0, 5), (0, 7) ] -- Sheep positions
