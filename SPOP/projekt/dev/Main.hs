module Main where

import Logic.TicTacToe
import Logic.GameTree

main :: IO ()
main = putStrLn (show test)

board :: Board
board = Board [
				[Empty, Empty, Empty],
				[Empty, Empty, Empty],
				[Empty, Empty, Empty]
			]

board1 :: Board
board1 = Board [
				[Cross, Circle, Circle],
				[Cross, Empty, Circle],
				[Empty, Cross, Empty]
			]

test = buildGameTree board1 Crosses
