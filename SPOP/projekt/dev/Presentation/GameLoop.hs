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
					   	"1"    -> nextTurn board (0, 0)
					   	"2"    -> nextTurn board (0, 1)
					   	"3"    -> nextTurn board (0, 2)
					   	"4"    -> nextTurn board (1, 0)
					   	"5"    -> nextTurn board (1, 1)
					   	"6"    -> nextTurn board (1, 2)
					   	"7"    -> nextTurn board (2, 0)
					   	"8"    -> nextTurn board (2, 1)
					   	"9"    -> nextTurn board (2, 2)
					   	"save" -> do
			   						 saveGame board
			   						 runGameLoop board
					   	"load" -> do
			   						 loadedBoard <- loadGame
			   						 runGameLoop loadedBoard
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



-- nextTurn function
nextTurn :: Board -> (Int, Int) -> IO ()
nextTurn board (row, col) = do
								move <- return (Move Cross (row, col))
								if (isMoveAllowed move board)
									then do 
											playerResult <- return (applyMove move board)
											aiResult <- return (aiMove playerResult Circles)
											runGameLoop aiResult
									else error "Error: move not allowed"
