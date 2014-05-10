module Logic.TicTacToe(
	Player(Crosses, Circles), getPlayerOpponent,
	Field(Empty, Cross, Circle),
	Board(Board), getMoves, getScore)
	where

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

field :: Board -> (Int, Int) -> Field
field board (x, y) = ((fields board) !! y) !! x

size :: Board -> Int
size board = length (fields board)



-- getMoves function
getMoves :: Board -> Player -> [Board]
getMoves board player = [applyMove (getPlayerField player) f board | f <- getEmptyFields board]

getEmptyFields :: Board -> [(Int, Int)]
getEmptyFields board = [(x, y) | x <- [0 .. boardSize - 1], y <- [0 .. boardSize - 1], field board (x, y) == Empty]
						where boardSize = size board

applyMove :: Field -> (Int, Int) -> Board -> Board
applyMove f (x, y) board = Board [ [ if (row == x && col == y) then f else field board (row, col) | row <- [0 .. boardSize - 1]] | col <- [0 .. boardSize - 1] ]
							where boardSize = size board



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
getLineValues board = [map (\f -> field board f) line | line <- getBoardLines board]

getBoardLines :: Board -> [[(Int, Int)]]
getBoardLines board = [
							[(x, x) | x <- [0 .. boardSize - 1]],                               -- left-right diagonal
							[(x, boardSize - x - 1) | x <- [0 .. boardSize - 1]]                -- right-left diagonal
			  			]
						++ [ [(x, y) | x <- [0 .. boardSize - 1]] | y <- [0 .. boardSize - 1] ] -- rows
						++ [ [(x, y) | y <- [0 .. boardSize - 1]] | x <- [0 .. boardSize - 1] ] -- columns
			  			where boardSize = size board
