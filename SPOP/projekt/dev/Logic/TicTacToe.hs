module Logic.TicTacToe
	where

{-(
	Player(Crosses, Circles), getPlayerOpponent,
	Field(Empty, Cross, Circle),
	Board(Board), getMoves, getScore)-}

import Data.List

-- Player type
data Player = Crosses | Circles deriving Eq

getPlayerOpponent :: Player -> Player
getPlayerOpponent Crosses = Circles
getPlayerOpponent Circles = Crosses



-- Field type
data Field = Empty | Cross | Circle deriving Eq	

instance Show Field where
    show Empty = "-"
    show Cross = "X"
    show Circle = "O"

getPlayerField :: Player -> Field
getPlayerField Crosses = Cross
getPlayerField Circles = Circle



-- Board type
data Board = Board { fields :: [[Field]] }
instance Show Board where
	show board = intercalate "\n" (map show (fields board))

size :: Board -> Int
size board = length (fields board)

getField :: Board -> (Int, Int) -> Field
getField board (column, row) = ((fields board) !! row) !! column



-- getMoves function
getMoves :: Board -> Player -> [Board]
getMoves board player = [applyMove (getPlayerField player) field board | field <- getEmptyFields board]

getEmptyFields :: Board -> [(Int, Int)]
getEmptyFields board = [(col, row) | col <- axis, row <- axis, getField board (col, row) == Empty]
						where axis = [0 .. (size board) - 1]

applyMove :: Field -> (Int, Int) -> Board -> Board
applyMove field (col, row) board = Board [ [ if (x == col && y == row) then field else getField board (x, y) | x <- axis] | y <- axis ]
									where axis = [0 .. (size board) - 1]



-- getScore function
getScore :: Board -> Player -> Int
getScore board player = if (hasWon player board)
							then 1
						else if (hasWon (getPlayerOpponent player) board)
							then -1
						else 0

hasWon :: Player -> Board -> Bool
hasWon player board = any (==True) [all (==(getPlayerField player)) line | line <- (getLineValues board)]

getLineValues :: Board -> [[Field]]
getLineValues board = [map (\field -> getField board field) line | line <- getBoardLines board]

getBoardLines :: Board -> [[(Int, Int)]]
getBoardLines board = [ [(col, row) | col <- [0 .. boardSize - 1]] | row <- [0 .. boardSize - 1] ] ++ -- rows
					  [ [(col, row) | row <- [0 .. boardSize - 1]] | col <- [0 .. boardSize - 1] ] ++ -- columns
					  [
						[(x, x) | x <- [0 .. boardSize - 1]],                                         -- left-right diagonal
						[(x, boardSize - x - 1) | x <- [0 .. boardSize - 1]]                          -- right-left diagonal
			  		  ]
			  			where boardSize = size board
