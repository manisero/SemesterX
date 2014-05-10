module Logic.TicTacToe where
import Data.List

-- Player type
data Player = Crosses | Circles deriving Eq

playerOpponent :: Player -> Player
playerOpponent Crosses = Circles
playerOpponent Circles = Crosses



-- Field type
data Field = Empty | Cross | Circle deriving Eq	

instance Show Field where
    show Empty = "-"
    show Cross = "X"
    show Circle = "O"

playerField :: Player -> Field
playerField Crosses = Cross
playerField Circles = Circle



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
getMoves board player = [apply (playerField player) f board | f <- emptyFields board]

emptyFields :: Board -> [(Int, Int)]
emptyFields board = [(x, y) | x <- [0 .. boardSize - 1], y <- [0 .. boardSize - 1], field board (x, y) == Empty]
					where boardSize = size board

apply :: Field -> (Int, Int) -> Board -> Board
apply f (x, y) board = Board [ [ if (row == x && col == y) then f else field board (row, col) | row <- [0 .. boardSize - 1]] | col <- [0 .. boardSize - 1] ]
					   where boardSize = size board



-- getScore function
getScore :: Board -> Player -> Int
getScore board player = if (hasWon player board)
						 	then 1
						 else if (hasWon (playerOpponent player) board)
						 	then -1
						 else 0

hasWon :: Player -> Board -> Bool
hasWon player board = any (==True) [all (==(playerField player)) line | line <- (lineValues board)]

lineValues :: Board -> [[Field]]
lineValues board = [map (\f -> field board f) line | line <- boardLines board]

boardLines :: Board -> [[(Int, Int)]]
boardLines board = [
				[(x, x) | x <- [0 .. boardSize - 1]],                                 -- left-right diagonal
				[(x, boardSize - x - 1) | x <- [0 .. boardSize - 1]]                  -- right-left diagonal
			  ]
			  ++ [ [(x, y) | x <- [0 .. boardSize - 1]] | y <- [0 .. boardSize - 1] ] -- rows
			  ++ [ [(x, y) | y <- [0 .. boardSize - 1]] | x <- [0 .. boardSize - 1] ] -- columns
			  where boardSize = size board
