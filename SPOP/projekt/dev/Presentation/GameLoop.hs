module Presentation.GameLoop(runGameLoop) where

import Logic.Game
import Logic.AI
import Presentation.Board
import Presentation.SaveLoad

-- runGameLoop function
runGameLoop :: Board -> IO ()
runGameLoop board = do
					   printState board
					   input <- getLine
					   case input of
					   	"1"    -> move board (0, 0)
					   	"2"    -> move board (0, 1)
					   	"3"    -> move board (0, 2)
					   	"4"    -> move board (1, 0)
					   	"5"    -> move board (1, 1)
					   	"6"    -> move board (1, 2)
					   	"7"    -> move board (2, 0)
					   	"8"    -> move board (2, 1)
					   	"9"    -> move board (2, 2)
					   	"save" -> do
			   						 saveGame board
			   						 runGameLoop board
					   	"load" -> do
			   						 board <- loadGame
			   						 runGameLoop board
					   	"exit" -> return ()
					   	_      -> runGameLoop board



-- printState function
printState :: Board -> IO ()
printState board = do
					  putStrLn ""
					  putStrLn "============="
					  putStrLn ""
					  putStrLn (printBoard board)
					  putStrLn ""
					  putStrLn "1 - 9 - move"
					  putStrLn "save - save game"
					  putStrLn "load - load game"
					  putStrLn "exit - exit"
					  putStrLn ""
					  putStrLn "command:"



-- move function
move :: Board -> (Int, Int) -> IO ()
move board field = runGameLoop (aiMove (applyMove Cross field board) Circles)
