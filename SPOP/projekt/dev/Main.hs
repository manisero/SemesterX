module Main(main) where

import Logic.Game_TicTacToe
import Presentation.GameLoop

main :: IO ()
main = startGame emptyBoard Crosses

emptyBoard :: Board
emptyBoard = Board [
					[Empty, Empty, Empty],
					[Empty, Empty, Empty],
					[Empty, Empty, Empty]
				]
