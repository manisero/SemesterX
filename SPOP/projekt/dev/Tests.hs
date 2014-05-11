module Main where

import System.CPUTime
import Logic.Game
import Logic.GameTree

main :: IO ()
main = test

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
		before1 <- getCPUTime
		putStrLn (show (Logic.GameTree.getScore (buildGameTree emptyBoard Crosses)))
		displayTimePassedSince before1
		before2 <- getCPUTime
		putStrLn (show (getStateScore emptyBoard Crosses Crosses))
		displayTimePassedSince before2

displayTimePassedSince timeStamp = do
									now <- getCPUTime
									putStrLn (show ((fromInteger (now - timeStamp)::Float) / 10^12))
