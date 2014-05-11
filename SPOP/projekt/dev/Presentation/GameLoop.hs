module Presentation.GameLoop(gameLoop) where

import Logic.Game
import Logic.AI
import Presentation.Board
import Presentation.SaveLoad

-- gameLoop function
gameLoop :: Board -> Player -> Player -> IO ()
gameLoop board currentPlayer humanPlayer = do
											printNewTurn board
											let result = getResult board humanPlayer
											if (result == Unsettled)
												then do
													putStrLn ("Turn of " ++ show currentPlayer)
													if (currentPlayer == humanPlayer) 
														then do
															printOptions
															input <- getLine
															processHumanInput input board currentPlayer
														else do
															let afterAiMove = aiMove board currentPlayer
															gameLoop afterAiMove (getPlayerOpponent currentPlayer) humanPlayer
												else putStrLn (show result ++ "!")



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
				  putStrLn "exit - exit"
				  putStrLn ""
				  putStrLn "command:"



-- processHumanInput function
processHumanInput :: String -> Board -> Player -> IO ()
processHumanInput input board player = case input of
										"save" -> do
													saveGame board
													gameLoop board player player
										"load" -> do
													loadedBoard <- loadGame
													gameLoop loadedBoard player player
										"exit" -> return ()
										_      -> gameLoop (processMoveCommand input board player) (getPlayerOpponent player) player



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
