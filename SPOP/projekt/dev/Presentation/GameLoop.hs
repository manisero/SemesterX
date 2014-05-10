module Presentation.GameLoop(runGameLoop) where

import Data.List
import Logic.Game
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



-- printState function
printState :: Board -> IO ()
printState board = do
					  putStrLn ""
					  putStrLn "============="
					  putStrLn ""
					  putStrLn (presentBoard board)
					  putStrLn ""
					  putStrLn "0 - 9 - move"
					  putStrLn "save - save game"
					  putStrLn "load - load game"
					  putStrLn "exit - exit"
					  putStrLn ""
					  putStrLn "command:"

presentBoard :: Board -> String
presentBoard board = intercalate "\n" [ show [ presentField field | field <- row ] | row <- fields board ]

presentField :: Field -> String
presentField Empty = "-"
presentField Cross = "X"
presentField Circle = "O"
