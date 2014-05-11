module Presentation.GameLoop(gameLoop) where

import Logic.Game
import Logic.AI
import Presentation.Board
import Presentation.SaveLoad

-- gameLoop function
gameLoop :: Board -> Player -> Player -> IO ()
gameLoop board currentPlayer humanPlayer = do
											let result = getResult board humanPlayer
											if (result == Unsettled)
												then do
													printNewTurn currentPlayer board
													if (currentPlayer == humanPlayer) 
														then do
															printOptions
															input <- getLine
															processHumanInput input board currentPlayer
														else do
															let afterAiMove = aiMove board currentPlayer
															gameLoop afterAiMove opponent humanPlayer
												else putStrLn (show result ++ "!")
											where opponent = getPlayerOpponent currentPlayer



-- printNewTurn function
printNewTurn :: Player -> Board -> IO ()
printNewTurn player board = do
								putStrLn ""
								putStrLn "============="
								putStrLn ""
								putStrLn ("Turn of " ++ show player)
								putStrLn ""
								printBoard board



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
										"1"    -> applyHumanMove player board (0, 0)
										"2"    -> applyHumanMove player board (0, 1)
										"3"    -> applyHumanMove player board (0, 2)
										"4"    -> applyHumanMove player board (1, 0)
										"5"    -> applyHumanMove player board (1, 1)
										"6"    -> applyHumanMove player board (1, 2)
										"7"    -> applyHumanMove player board (2, 0)
										"8"    -> applyHumanMove player board (2, 1)
										"9"    -> applyHumanMove player board (2, 2)
										"save" -> do
													saveGame board
													gameLoop board player player
										"load" -> do
													loadedBoard <- loadGame
													gameLoop loadedBoard player player
										"exit" -> return ()
										_      -> error "Invalid command"



-- applyHumanMove function
applyHumanMove :: Player -> Board -> (Int, Int) -> IO ()
applyHumanMove player board field = do
										let move = Move (getPlayerField player) field
										if (isMoveAllowed move board)
											then do 
												let afterMove = applyMove move board
												gameLoop afterMove (getPlayerOpponent player) player
											else error "Move not allowed"
