module Main where
import Logic.TicTacToe
import Logic.GameTree

main :: IO ()
main = putStrLn (show (buildGameTree board1 Crosses))

board :: Board
board = Board [
				[Empty, Empty, Empty],
				[Empty, Empty, Empty],
				[Empty, Empty, Empty]
			]

board1 :: Board
board1 = Board [
				[Empty, Circle, Empty],
				[Cross, Cross, Circle],
				[Circle, Cross, Empty]
			]