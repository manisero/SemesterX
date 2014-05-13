module Presentation.GameLoop_WolfNSheep(
	printMoveOptions,
	printBoard,
	processMoveCommand)
	where

import Data.List
import Logic.Game_WolfNSheep

-- printMoveOptions function
printMoveOptions :: IO ()
printMoveOptions = putStrLn "[move options]"



-- printBoard function
printBoard :: Board -> IO ()
printBoard board = putStrLn (showBoard board)

showBoard :: Board -> String
showBoard board = intercalate "\n" [ [ if ((row, column) == getWolfPosition board) 
									   then 'W'
									   else if (elem (row, column) (getSheepPositions board))
										then 'S'
										else if (even (row + column))
											then '-'
											else ' '
									   | column <- axis ]
									 | row <- axis ]
					where axis = [ 0 .. (getSize board) - 1 ]



-- processMoveCommand function
processMoveCommand :: String -> Board -> Player -> Board
processMoveCommand command board player = case command of
											{-
											"1"    -> applyHumanMove player board (0, 0)
											"2"    -> applyHumanMove player board (0, 1)
											"3"    -> applyHumanMove player board (0, 2)
											"4"    -> applyHumanMove player board (1, 0)
											"5"    -> applyHumanMove player board (1, 1)
											"6"    -> applyHumanMove player board (1, 2)
											"7"    -> applyHumanMove player board (2, 0)
											"8"    -> applyHumanMove player board (2, 1)
											"9"    -> applyHumanMove player board (2, 2)
											-}
											_      -> error "Invalid command"

{-
applyHumanMove :: Player -> Board -> (Int, Int) -> Board
applyHumanMove player board field = do
										let move = Move (getPlayerField player) field
										if (isMoveAllowed move board)
											then applyMove move board
											else error "Move not allowed"
-}
