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

filledBoard :: Board
filledBoard = Board [
					[Cross, Circle, Circle],
					[Empty, Empty, Circle],
					[Empty, Cross, Empty]
				]

testBoard :: Board
testBoard = emptyBoard



--test = buildGameTree testBoard Crosses
--test = (read "Board {fields = [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]}")::Board
--test = getStateScore emptyBoard Crosses Crosses
test = do
		putStrLn "Alpha-Beta:"
		before3 <- getCPUTime
		putStrLn (show (alphaBeta testBoard Crosses Crosses (minBound::Int) (maxBound::Int)))
		displayTimePassedSince before3
		putStrLn "minimax:"
		before2 <- getCPUTime
		putStrLn (show (minimax testBoard Crosses Crosses))
		displayTimePassedSince before2
		putStrLn "Game tree:"
		before1 <- getCPUTime
		putStrLn (show (Logic.GameTree.getScore (buildGameTree testBoard Crosses)))
		displayTimePassedSince before1



displayTimePassedSince timeStamp = do
									now <- getCPUTime
									putStrLn (show ((fromInteger (now - timeStamp)::Float) / 10^12))
