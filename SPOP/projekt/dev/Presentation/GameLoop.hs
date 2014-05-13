module Presentation.GameLoop(startGame) where

import Logic.Game_WolfNSheep
import Logic.AI.Heuristic
import Presentation.GameLoop_WolfNSheep
import Presentation.SaveLoad

-- startGame function
startGame :: Board -> Player -> IO ()
startGame emptyBoard humanPlayer = gameLoop emptyBoard humanPlayer humanPlayer emptyBoard


-- gameLoop function
gameLoop :: Board -> Player -> Player -> Board -> IO ()
gameLoop board currentPlayer humanPlayer emptyBoard = do
														printNewTurn board
														let result = getResult board humanPlayer
														if (result == Unsettled)
															then do
																putStrLn ("Turn of " ++ show currentPlayer)
																if (currentPlayer == humanPlayer) 
																	then do
																		printOptions
																		input <- getLine
																		case input of
																			"save"    -> do
																							saveGame board
																							gameLoop board currentPlayer humanPlayer emptyBoard
																			"load"    -> do
																							loadedBoard <- loadGame
																							gameLoop loadedBoard humanPlayer humanPlayer emptyBoard
																			"restart" -> startGame emptyBoard humanPlayer
																			"exit"    -> return ()
																			_         -> gameLoop (processMoveCommand input board currentPlayer) opponent humanPlayer emptyBoard
																	else do
																		let afterAiMove = aiMove board currentPlayer
																		gameLoop afterAiMove opponent humanPlayer emptyBoard
															else putStrLn (show result ++ "!")
														where opponent = getPlayerOpponent currentPlayer


-- printNewTurn function
printNewTurn :: Board -> IO ()
printNewTurn board = do
						putStrLn ""
						putStrLn "================"
						putStrLn ""
						printBoard board
						putStrLn ""
								

-- printOptions function
printOptions :: IO ()
printOptions = do
				  putStrLn ""
				  printMoveOptions
				  putStrLn "save - save game"
				  putStrLn "load - load game"
				  putStrLn "restart - restart game"
				  putStrLn "exit - exit"
				  putStrLn ""
				  putStrLn "command:"
