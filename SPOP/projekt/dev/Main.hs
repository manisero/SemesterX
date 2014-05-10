module Main where
import Logic.Board
import Logic.GameTree
import Logic.TicTacToe

main :: IO ()
main = putStrLn (show (buildGameTree board1 Crosses))

board :: TTTBoard
board = TTTBoard [
					[Empty, Empty, Empty],
					[Empty, Empty, Empty],
					[Empty, Empty, Empty]
				]

board1 :: TTTBoard
board1 = TTTBoard [
					[Empty, Circle, Empty],
					[Cross, Cross, Circle],
					[Circle, Cross, Empty]
				]