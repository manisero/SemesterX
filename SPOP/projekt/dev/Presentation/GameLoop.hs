module Presentation.GameLoop(startGame) where

import Logic.Game
import Logic.AI.Heuristic
import Presentation.Print
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
				  putStrLn "1 - 9 - move"
				  putStrLn "save - save game"
				  putStrLn "load - load game"
				  putStrLn "restart - restart game"
				  putStrLn "exit - exit"
				  putStrLn ""
				  putStrLn "command:"



-- processMoveCommand function
processMoveCommand :: String -> Board -> Player -> Board
processMoveCommand command board player = case command of
											"1"    -> applyHumanMove player board (0, 0)
											"2"    -> applyHumanMove player board (0, 1)
											"3"    -> applyHumanMove player board (0, 2)
											"4"    -> applyHumanMove player board (1, 0)
											"5"    -> applyHumanMove player board (1, 1)
											"6"    -> applyHumanMove player board (1, 2)
											"7"    -> applyHumanMove player board (2, 0)
											"8"    -> applyHumanMove player board (2, 1)
											"9"    -> applyHumanMove player board (2, 2)
											_      -> error "Invalid command"



-- applyHumanMove function
applyHumanMove :: Player -> Board -> (Int, Int) -> Board
applyHumanMove player board field = do
										let move = Move (getPlayerField player) field
										if (isMoveAllowed move board)
											then applyMove move board
											else error "Move not allowed"
