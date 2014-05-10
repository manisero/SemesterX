module Main where
import Board

main :: IO ()
main = putStrLn (show (fields board))

board :: Board
board = Board [
				[Empty, Empty, Empty],
				[Empty, Empty, Empty],
				[Empty, Empty, Empty]
			]
