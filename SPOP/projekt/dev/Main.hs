module Main where

import Logic.TicTacToe
import Logic.GameTree
import Presentation.GameLoop

main :: IO ()
main = runGameLoop emptyBoard

emptyBoard :: Board
emptyBoard = Board [
					[Empty, Empty, Empty],
					[Empty, Empty, Empty],
					[Empty, Empty, Empty]
				]

testBoard :: Board
testBoard = Board [
					[Cross, Circle, Circle],
					[Cross, Empty, Circle],
					[Empty, Cross, Empty]
				]

test = buildGameTree emptyBoard Crosses
