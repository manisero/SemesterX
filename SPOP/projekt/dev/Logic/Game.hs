module Logic.Game(
	Player(Crosses, Circles), getPlayerOpponent,
	Field(Empty, Cross, Circle),
	Move(Move),
	Board(Board, fields), getMoves, getScore, applyMove)
	where

-- Player type
data Player = Crosses | Circles deriving Eq

getPlayerOpponent :: Player -> Player
getPlayerOpponent Crosses = Circles
getPlayerOpponent Circles = Crosses



-- Field type
data Field = Empty | Cross | Circle deriving (Eq, Show, Read)

getPlayerField :: Player -> Field
getPlayerField Crosses = Cross
getPlayerField Circles = Circle



-- Move type
data Move = Move Field (Int, Int)



-- Board type
data Board = Board { fields :: [[Field]] } deriving (Show, Read)

size :: Board -> Int
size board = length (fields board)

getField :: Board -> (Int, Int) -> Field
getField board (row, column) = ((fields board) !! row) !! column



-- getMoves function
getMoves :: Board -> Player -> [Board]
getMoves board player = [applyMove (Move (getPlayerField player) field) board | field <- getEmptyFields board]

getEmptyFields :: Board -> [(Int, Int)]
getEmptyFields board = [(row, col) | row <- axis, col <- axis, getField board (row, col) == Empty]
						where axis = [0 .. (size board) - 1]



-- applyMove function
applyMove :: Move -> Board -> Board
applyMove (Move field (row, col)) board = Board [ [ if (y == row && x == col) then field else getField board (y, x) | x <- axis] | y <- axis ]
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
getBoardLines board = [ [(row, col) | col <- axis] | row <- axis ] ++ -- rows
					  [ [(row, col) | row <- axis] | col <- axis ] ++ -- columns
					  [
						[(x, x) | x <- axis],                         -- left-right diagonal
						[(x, (size board) - x - 1) | x <- axis]       -- right-left diagonal
			  		  ]
			  			where axis = [0 .. (size board) - 1]
