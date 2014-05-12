module Logic.Game_TicTacToe(
	GameResult(Unsettled, Victory, Defeat, Draw),
	Player(Crosses, Circles), getPlayerOpponent,
	Field(Empty, Cross, Circle),getPlayerField,
	Move(Move),
	Board(Board, getFields), getMoves, isMoveAllowed, applyMove, getScore, getResult)
	where

-- GameResult type
data GameResult = Unsettled | Victory | Defeat | Draw deriving (Eq, Show)



-- Player type
data Player = Crosses | Circles deriving (Eq, Show)

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
data Board = Board { getFields :: [[Field]] } deriving (Show, Read)

getSize :: Board -> Int
getSize board = length (getFields board)

getField :: Board -> (Int, Int) -> Field
getField board (row, column) = ((getFields board) !! row) !! column



-- getMoves function
getMoves :: Board -> Player -> [Board]
getMoves board player = if (hasWon player board || hasWon (getPlayerOpponent player) board)
						then []
						else [applyMove (Move (getPlayerField player) field) board | field <- getEmptyFields board]
						

getEmptyFields :: Board -> [(Int, Int)]
getEmptyFields board = [(row, col) | row <- axis, col <- axis, getField board (row, col) == Empty]
						where axis = [0 .. (getSize board) - 1]


-- isMoveAllowed function
isMoveAllowed :: Move -> Board -> Bool
isMoveAllowed (Move _ (row, col)) board = (getField board (row, col)) == Empty


-- applyMove function
applyMove :: Move -> Board -> Board
applyMove (Move field (row, col)) board = Board [ [ if (y == row && x == col) then field else getField board (y, x) | x <- axis] | y <- axis ]
											where axis = [0 .. (getSize board) - 1]



-- getScore and getResult functions
getScore :: Board -> Player -> Int
getScore board player = if (hasWon player board)
							then 1
						else if (hasWon (getPlayerOpponent player) board)
							then -1
						else 0


getResult :: Board -> Player -> GameResult
getResult board player = case getScore board player of
							1  -> Victory
							-1 -> Defeat
							_  -> if (length (getMoves board player) == 0)
						 		  then Draw
						 		  else Unsettled


hasWon :: Player -> Board -> Bool
hasWon player board = any (==True) [all (==(getPlayerField player)) line | line <- (getLineValues board)]

getLineValues :: Board -> [[Field]]
getLineValues board = [map (\field -> getField board field) line | line <- getBoardLines board]

getBoardLines :: Board -> [[(Int, Int)]]
getBoardLines board = [ [(row, col) | col <- axis] | row <- axis ] ++ -- rows
					  [ [(row, col) | row <- axis] | col <- axis ] ++ -- columns
					  [
						[(x, x) | x <- axis],                         -- left-right diagonal
						[(x, (getSize board) - x - 1) | x <- axis]    -- right-left diagonal
			  		  ]
			  			where axis = [0 .. (getSize board) - 1]
