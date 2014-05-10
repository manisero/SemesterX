module Presentation.GameLoop(runGameLoop) where

import Logic.TicTacToe
import Presentation.SaveLoad

-- runGameLoop function
runGameLoop :: Board -> IO ()
runGameLoop board = do
					   printState board
					   input <- getLine
					   case input of
					   	"save" -> do
			   						 saveGame board
			   						 runGameLoop board
					   	"load" -> do
			   						 board <- loadGame
			   						 runGameLoop board
					   	"exit" -> return ()
					   	_      -> runGameLoop board



printState :: Board -> IO ()
printState board = do
					  putStrLn ""
					  putStrLn "============="
					  putStrLn ""
					  putStrLn (show board)
					  putStrLn ""
					  putStrLn "0 - 9 - move"
					  putStrLn "save - save game"
					  putStrLn "load - load game"
					  putStrLn "exit - exit"
					  putStrLn ""
					  putStrLn "============="
					  putStrLn ""
					  putStrLn "command:"
