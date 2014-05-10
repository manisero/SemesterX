module Logic.TicTacToe where
import Logic.Board
import Data.List

playerOpponent :: Player -> Player
playerOpponent Crosses = Circles
playerOpponent Circles = Crosses

playerField :: Player -> Field
playerField Crosses = Cross
playerField Circles = Circle

data TTTBoard = TTTBoard [[Field]]
instance Board TTTBoard where
	fields (TTTBoard fs) = fs
	getScore board player = if (hasWon player board)
						 	then 1
						 else if (hasWon (playerOpponent player) board)
						 	then -1
						 else 0
	getMoves board player = [apply (playerField player) f board | f <- emptyFields board]

instance Show TTTBoard where
	show board = intercalate "\n" (map show (fields board))

hasWon :: Player -> TTTBoard -> Bool
hasWon player board = any (==True) [all (==(playerField player)) line | line <- (lineValues board)]

lineValues :: TTTBoard -> [[Field]]
lineValues board = [map (\f -> field board f) line | line <- boardLines board]

boardLines :: TTTBoard -> [[(Int, Int)]]
boardLines board = [
				[(x, x) | x <- [0 .. boardSize - 1]],                                 -- left-right diagonal
				[(x, boardSize - x - 1) | x <- [0 .. boardSize - 1]]                  -- right-left diagonal
			  ]
			  ++ [ [(x, y) | x <- [0 .. boardSize - 1]] | y <- [0 .. boardSize - 1] ] -- rows
			  ++ [ [(x, y) | y <- [0 .. boardSize - 1]] | x <- [0 .. boardSize - 1] ] -- columns
			  where boardSize = size board

emptyFields :: TTTBoard -> [(Int, Int)]
emptyFields board = [(x, y) | x <- [0 .. boardSize - 1], y <- [0 .. boardSize - 1], field board (x, y) == Empty]
					where boardSize = size board

apply :: Field -> (Int, Int) -> TTTBoard -> TTTBoard
apply f (x, y) board = TTTBoard [ [ if (row == x && col == y) then f else field board (row, col) | row <- [0 .. boardSize - 1]] | col <- [0 .. boardSize - 1] ]
					   where boardSize = size board