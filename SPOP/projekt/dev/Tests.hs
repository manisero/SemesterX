module Main where

import System.CPUTime
import Data.Maybe
import Logic.Game
import Logic.AI
import Presentation.GameLoop_WolfNSheep

main :: IO ()
main = testAiMovesPerformance

testBoard :: Board
testBoard = Board 8 (6, 3) [ (0, 1), (0, 3), (0, 5), (0, 7) ]



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
		testAiMovePerformance 9
		testAiMovePerformance 10
		testAiMovePerformance 11
		testAiMovePerformance 12
		testAiMovePerformance 13

testAiMovePerformance :: Int -> IO ()
testAiMovePerformance moves = do
								putStrLn ((show moves) ++ " moves:")
								putStrLn ""
								before <- getCPUTime
								printBoard (fromJust (aiMove_customDepth testBoard Sheep moves))
								putStrLn ""
								displayTimePassedSince before
								putStrLn ""
								putStrLn ""
								putStrLn ""

displayTimePassedSince :: Integer -> IO ()
displayTimePassedSince timeStamp = do
									now <- getCPUTime
									putStrLn (show ((fromInteger (now - timeStamp)::Float) / 10 ^ (12::Integer)))
