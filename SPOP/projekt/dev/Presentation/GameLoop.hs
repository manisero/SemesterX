module Presentation.GameLoop(startGame) where

import Data.Maybe
import Logic.Game
import Logic.AI
import Presentation.GameSpecific
import Presentation.SaveLoad

-- startGameFunction
startGame :: Board -> Player -> IO ()
startGame emptyBoard humanPlayer = do
									putStrLn "================"
									putStrLn ""
									putStrLn "start - start new game"
									putStrLn "load - load game"
									putStrLn ""
									putStrLn "command:"
									input <- getLine
									case input of
										"start" -> startNewGame emptyBoard humanPlayer
										"load"  -> do
													loadedBoard <- loadGame
													if (isJust loadedBoard)
														then gameLoop (fromJust loadedBoard) humanPlayer humanPlayer emptyBoard
														else do
															putStrLn ""
															startGame emptyBoard humanPlayer
										_       -> do
														putStrLn ""
														putStrLn "Invalid command"
														putStrLn ""
														startGame emptyBoard humanPlayer



-- startNewGame function
startNewGame :: Board -> Player -> IO ()
startNewGame emptyBoard humanPlayer = do
									board <- initializeBoard emptyBoard
									if (isJust (board))
										then gameLoop (fromJust board) humanPlayer humanPlayer emptyBoard
										else do
											putStrLn ""
											putStrLn "Invalid command"
											putStrLn ""
											startNewGame emptyBoard humanPlayer



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
																							if (isJust loadedBoard)
																								then gameLoop (fromJust loadedBoard) humanPlayer humanPlayer emptyBoard
																								else gameLoop board humanPlayer humanPlayer emptyBoard
																							
																			"restart" -> startNewGame emptyBoard humanPlayer
																			"exit"    -> return ()
																			_         -> case processMoveCommand input board of
																							Just afterHumanMove -> gameLoop afterHumanMove opponent humanPlayer emptyBoard
																							Nothing             -> do
																													putStrLn ""
																													putStrLn "Invalid command"
																													gameLoop board currentPlayer humanPlayer emptyBoard
																	else do
																		let afterAiMove = aiMove board currentPlayer
																		if (isJust afterAiMove)
																			then gameLoop (fromJust afterAiMove) opponent humanPlayer emptyBoard
																			else do
																					putStrLn ""
																					putStrLn (show currentPlayer ++ " cannot move")
																					gameLoop board opponent humanPlayer emptyBoard
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
