module Main where
import Board

main :: IO ()
main = putStrLn (show (score board1 Crosses))

board :: TTTBoard
board = TTTBoard [
					[Empty, Empty, Empty],
					[Empty, Empty, Empty],
					[Empty, Empty, Empty]
				]

board1 :: TTTBoard
board1 = TTTBoard [
					[Cross, Circle, Circle],
					[Cross, Empty, Circle],
					[Cross, Cross, Empty]
				]