module Presentation.GameLoop_WolfNSheep(
	printMoveOptions,
	printBoard,
	processMoveCommand)
	where

import Data.List
import Logic.Game_WolfNSheep

-- printMoveOptions function
printMoveOptions :: IO ()
printMoveOptions = do
					putStrLn "q - move top left"
					putStrLn "w - move top right"
					putStrLn "a - move bottom left"
					putStrLn "s - move bottom right"



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
processMoveCommand :: String -> Board -> Board
processMoveCommand command board = case command of
											"q"    -> applyHumanMove topLeftOf board
											"w"    -> applyHumanMove topRightOf board
											"a"    -> applyHumanMove bottomLeftOf board
											"s"    -> applyHumanMove bottomRightOf board
											_      -> error "Invalid command"


applyHumanMove :: (Field -> Field) -> Board -> Board
applyHumanMove transition board  = do
									let move = MoveWolf (transition (getWolfPosition board))
									if (isMoveAllowed move board)
										then applyMove move board
										else error "Move not allowed"
