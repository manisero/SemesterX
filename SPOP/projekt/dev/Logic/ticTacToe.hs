module Logic.TicTacToe where
import Logic.Board

playerOpponent :: Player -> Player
playerOpponent Crosses = Circles
playerOpponent Circles = Crosses

playerField :: Player -> Field
playerField Crosses = Cross
playerField Circles = Circle

data TTTBoard = TTTBoard [[Field]]
instance Board TTTBoard where
	fields (TTTBoard fs) = fs
	score board player = if (hasWon player board)
						 	then 1
						 else if (hasWon (playerOpponent player) board)
						 	then -1
						 else 0
	moves board player = [(x, y) | x <- [0 .. boardSize - 1], y <- [0 .. boardSize - 1], field board (x, y) == Empty]
						 where boardSize = size board

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