module Main where

--import System.CPUTime
import Logic.Game_WolfNSheep

main :: IO ()
main = putStrLn (show test)

testBoard :: Board
testBoard = Board 8 (7, 0) [ (0, 1), (0, 3), (0, 5), (0, 7) ]

test :: Board
test = applyMove (MoveSheep (0, 3) (3,3)) testBoard


{-
test = do
		putStrLn "Heuristic:"
		before4 <- getCPUTime
		putStrLn (show (alphaBetaHeuristic testBoard Crosses Crosses 8))
		displayTimePassedSince before4
		putStrLn "Alpha-Beta:"
		before3 <- getCPUTime
		putStrLn (show (alphaBeta testBoard Crosses Crosses))
		displayTimePassedSince before3
		putStrLn "minimax:"
		before2 <- getCPUTime
		putStrLn (show (minimax testBoard Crosses Crosses))
		displayTimePassedSince before2
		putStrLn "Game tree:"
		before1 <- getCPUTime
		putStrLn (show (Logic.AI.GameTree.getScore (buildGameTree testBoard Crosses)))
		displayTimePassedSince before1

displayTimePassedSince timeStamp = do
									now <- getCPUTime
									putStrLn (show ((fromInteger (now - timeStamp)::Float) / 10^12))
-}
