module Main where

import System.CPUTime
import Data.Maybe
import Logic.Game_WolfNSheep
import Logic.AI.Heuristic

main :: IO ()
main = test
--main = putStrLn (show test)

testBoard :: Board
testBoard = Board 8 (6, 3) [ (0, 1), (0, 3), (0, 5), (0, 7) ]

--test :: Board
--test = applyMove (MoveSheep (0, 3) (3,3)) testBoard


test :: IO ()
test = do
		testMoves 8
		testMoves 9
		testMoves 10
		testMoves 12



testMoves :: Int -> IO ()
testMoves moves = do
					putStrLn ((show moves) ++ " moves:")
					before <- getCPUTime
					putStrLn (show (fromJust (aiMove_customDepth testBoard Sheep moves)))
					displayTimePassedSince before
					putStrLn ""


displayTimePassedSince :: Integer -> IO ()
displayTimePassedSince timeStamp = do
									now <- getCPUTime
									putStrLn (show ((fromInteger (now - timeStamp)::Float) / 10 ^ (12::Integer)))
