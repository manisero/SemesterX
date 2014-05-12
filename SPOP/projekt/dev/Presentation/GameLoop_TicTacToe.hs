module Presentation.GameLoop_TicTacToe(
	printMoveOptions,
	printBoard,
	processMoveCommand)
	where

import Data.List
import Logic.Game_TicTacToe

-- printMoveOptions function
printMoveOptions :: IO ()
printMoveOptions = putStrLn "1 - 9 - move"



-- printBoard function
printBoard :: Board -> IO ()
printBoard board = putStrLn (intercalate "\n" [ show [ showField field | field <- row ] | row <- getFields board ])

showField :: Field -> String
showField Empty = "-"
showField Cross = "X"
showField Circle = "O"



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


applyHumanMove :: Player -> Board -> (Int, Int) -> Board
applyHumanMove player board field = do
										let move = Move (getPlayerField player) field
										if (isMoveAllowed move board)
											then applyMove move board
											else error "Move not allowed"
