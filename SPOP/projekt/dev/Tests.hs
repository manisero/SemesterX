module Main where

import System.CPUTime
import Data.Maybe
import Logic.Game_WolfNSheep
import Logic.AI.Heuristic

main :: IO ()
main = testAiMovesPerformance

testBoard :: Board
testBoard = Board 8 (7, 0) [ (0, 1), (0, 3), (0, 5), (0, 7) ]



----------------
-- getScore test
----------------

testGetScore :: IO ()
testGetScore = do
				putStrLn (show (getScore testBoard Sheep))



--------------------------
-- aiMove performance test
--------------------------

testAiMovesPerformance :: IO ()
testAiMovesPerformance = do
		testAiMovePerformance 8
		testAiMovePerformance 9
		testAiMovePerformance 10
		testAiMovePerformance 11
		testAiMovePerformance 12

testAiMovePerformance :: Int -> IO ()
testAiMovePerformance moves = do
								putStrLn ((show moves) ++ " moves:")
								before <- getCPUTime
								putStrLn (show (fromJust (aiMove_customDepth testBoard Sheep moves)))
								displayTimePassedSince before
								putStrLn ""

displayTimePassedSince :: Integer -> IO ()
displayTimePassedSince timeStamp = do
									now <- getCPUTime
									putStrLn (show ((fromInteger (now - timeStamp)::Float) / 10 ^ (12::Integer)))
