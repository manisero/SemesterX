module Main(main) where

import Logic.Game
import Presentation.GameLoop

main :: IO ()
main = gameLoop emptyBoard Crosses Crosses
--main = putStrLn (show test)

emptyBoard :: Board
emptyBoard = Board [
					[Empty, Empty, Empty],
					[Empty, Empty, Empty],
					[Empty, Empty, Empty]
				]

emptyBoard55 :: Board
emptyBoard55 = Board [
					[Empty, Empty, Empty, Empty, Empty],
					[Empty, Empty, Empty, Empty, Empty],
					[Empty, Empty, Empty, Empty, Empty],
					[Empty, Empty, Empty, Empty, Empty],
					[Empty, Empty, Empty, Empty, Empty]
				]

testBoard :: Board
testBoard = Board [
					[Cross, Circle, Circle],
					[Cross, Empty, Circle],
					[Empty, Cross, Empty]
				]

--test = buildGameTree testBoard Crosses
--test = (read "Board {fields = [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]}")::Board
test = testBoard
