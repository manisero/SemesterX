module Presentation.GameLoop_WolfNSheep(
	printBoard)
	where

import Data.List
import Logic.Game_WolfNSheep

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
