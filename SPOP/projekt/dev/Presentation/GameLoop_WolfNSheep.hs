module Presentation.GameLoop_WolfNSheep(
	initializeBoard,
	printMoveOptions,
	printBoard,
	processMoveCommand)
	where

import Data.Maybe
import Data.List
import Text.Read
import Logic.Game_WolfNSheep

-- initializeBoard function
initializeBoard :: Board -> IO Board
initializeBoard (Board size _ sheepPositions) = do
													putStrLn "================"
													putStrLn ""
													putStrLn ("Choose Wolf position (" ++ intercalate ", " [ show column | column <- [0 .. size - 1], even column ] ++ "):")
													input <- getLine
													let column = readMaybe input::Maybe Int
													return (getInitializedBoard column size sheepPositions)


getInitializedBoard :: Maybe Int -> Int -> [Field] -> Board
getInitializedBoard wolfColumn boardSize sheepPositions = if (isJust wolfColumn && column >= 0 && column < boardSize && even column)
														  then Board boardSize (boardSize - 1, column) sheepPositions
														  else error "Invalid command"
															where column = fromJust wolfColumn



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
