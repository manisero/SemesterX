module Presentation.GameSpecific(
	initializeBoard,
	printMoveOptions,
	printBoard,
	processMoveCommand)
	where

import Data.Maybe
import Data.List
import Text.Read
import Logic.Game

-- initializeBoard function
initializeBoard :: Board -> IO (Maybe Board)
initializeBoard (Board size _ sheepPositions) = do
													putStrLn "================"
													putStrLn ""
													putStrLn ("Choose Wolf position (" ++ intercalate ", " [ show (column + 1) | column <- [0 .. size - 1], even column ] ++ "):")
													input <- getLine
													let column = readMaybe input::Maybe Int
													return (getInitializedBoard column size sheepPositions)


getInitializedBoard :: Maybe Int -> Int -> [Field] -> Maybe Board
getInitializedBoard wolfColumn boardSize sheepPositions = if (isJust wolfColumn && column >= 0 && column < boardSize && even column)
														  then Just (Board boardSize (boardSize - 1, column) sheepPositions)
														  else Nothing
															where column = (fromJust wolfColumn) - 1



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
processMoveCommand :: String -> Board -> Maybe Board
processMoveCommand command board = case command of
											"q"    -> applyHumanMove topLeftOf board
											"w"    -> applyHumanMove topRightOf board
											"a"    -> applyHumanMove bottomLeftOf board
											"s"    -> applyHumanMove bottomRightOf board
											_      -> Nothing


applyHumanMove :: (Field -> Field) -> Board -> Maybe Board
applyHumanMove transition board  = do
									let move = MoveWolf (transition (getWolfPosition board))
									if (isMoveAllowed move board)
										then Just (applyMove move board)
										else Nothing
