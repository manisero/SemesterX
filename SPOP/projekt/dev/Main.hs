module Main(main) where

import Logic.Game
import Logic.GameTree
import Presentation.GameLoop

main :: IO ()
main = startGame emptyBoard Crosses
--main = putStrLn (show test)
--main = test

emptyBoard :: Board
emptyBoard = Board [
					[Empty, Empty, Empty],
					[Empty, Empty, Empty],
					[Empty, Empty, Empty]
				]

emptyBoard44 :: Board
emptyBoard44 = Board [
					[Empty, Empty, Empty, Empty],
					[Empty, Empty, Empty, Empty],
					[Empty, Empty, Empty, Empty],
					[Empty, Empty, Empty, Empty]
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
--test = getStateScore emptyBoard Crosses Crosses
test = do
		putStrLn (show (Logic.GameTree.getScore (buildGameTree emptyBoard44 Crosses)))
		putStrLn (show (getStateScore emptyBoard44 Crosses Crosses))
