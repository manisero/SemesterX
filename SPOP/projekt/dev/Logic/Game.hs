module Logic.Game(
	GameResult(Unsettled, Victory, Defeat),
	Player(Wolf, Sheep), getPlayerOpponent,
	Field, getRow, getColumn, translate, topLeftOf, topRightOf, bottomLeftOf, bottomRightOf,
	Move(MoveWolf, MoveSheep),
	Board(Board), getSize, getWolfPosition, getSheepPositions,
	isMoveAllowed, getMoves, applyMove,
	hasWon, getResult, getScore)
	where

-- GameResult type
data GameResult = Unsettled | Victory | Defeat deriving (Eq, Show)



-- Player type
data Player = Wolf | Sheep deriving (Eq, Show)

getPlayerOpponent :: Player -> Player
getPlayerOpponent Wolf = Sheep
getPlayerOpponent Sheep = Wolf



-- Field type
type Field = (Int, Int)

getRow :: Field -> Int
getRow (row, _) = row

getColumn :: Field -> Int
getColumn (_, column) = column

translate :: Field -> Int -> Int -> Field
translate (row, column) rowDelta columnDelta = (row + rowDelta, column + columnDelta)

topLeftOf :: Field -> Field
topLeftOf field = translate field (-1) (-1)

topRightOf :: Field -> Field
topRightOf field = translate field (-1) 1

bottomLeftOf :: Field -> Field
bottomLeftOf field = translate field 1 (-1)

bottomRightOf :: Field -> Field
bottomRightOf field = translate field 1 1



-- Move type
data Move = MoveWolf Field | MoveSheep Field Field deriving (Show)



-- Board type
data Board = Board {
					getSize :: Int,
					getWolfPosition :: Field,
					getSheepPositions :: [Field]
				   } deriving (Show, Read)



-- isMoveAllowed
isMoveAllowed :: Move -> Board -> Bool
isMoveAllowed (MoveWolf (row, col)) board = (row == wolfRow - 1 || row == wolfRow + 1) &&
											(col == wolfCol - 1 || col == wolfCol + 1) &&
											row >= 0 && row < boardSize &&
											col >= 0 && col < boardSize &&
											not (elem (row, col) (getSheepPositions board))
												where
													wolfPosition = getWolfPosition board
													wolfRow = getRow wolfPosition
													wolfCol = getColumn wolfPosition
													boardSize = getSize board

isMoveAllowed (MoveSheep (fromRow, fromCol) (toRow, toCol)) board = toRow == fromRow + 1 &&
																	(toCol == fromCol - 1 || toCol == fromCol + 1) &&
																	toRow < boardSize &&
																	toCol >= 0 && toCol < boardSize &&
																	destination /= (getWolfPosition board) &&
													 				not (elem destination (getSheepPositions board))
																		where
																			boardSize = getSize board
																			destination = (toRow, toCol)




-- getMoves function
getMoves :: Board -> Player -> [Move]
getMoves board Wolf = (if (isMoveAllowed topLeft     board) then [ topLeft ] else []) ++
					  (if (isMoveAllowed topRight    board) then [ topRight ] else []) ++
					  (if (isMoveAllowed bottomLeft  board) then [ bottomLeft ] else []) ++
					  (if (isMoveAllowed bottomRight board) then [ bottomRight ] else [])
						where
							wolfPosition = getWolfPosition board
							topLeft = MoveWolf (topLeftOf wolfPosition)
							topRight = MoveWolf (topRightOf wolfPosition)
							bottomLeft = MoveWolf (bottomLeftOf wolfPosition)
							bottomRight = MoveWolf (bottomRightOf wolfPosition)

getMoves board Sheep = concat [ (if (isMoveAllowed (bottomLeft sheep)  board) then [ bottomLeft sheep ]  else []) ++
					  	 		(if (isMoveAllowed (bottomRight sheep) board) then [ bottomRight sheep ] else [])
					  	 		| sheep <- getSheepPositions board ]
							where
								bottomLeft from = MoveSheep from (bottomLeftOf from)
								bottomRight from = MoveSheep from (bottomRightOf from)



-- applyMove function
applyMove :: Move -> Board -> Board
applyMove (MoveWolf field) (Board size _ sheepPositions) = Board size field sheepPositions
applyMove (MoveSheep from to) (Board size wolfPosition sheepPositions) = Board size wolfPosition (replaceSheep from to sheepPositions)

replaceSheep :: Field -> Field -> [Field] -> [Field]
replaceSheep old new (field:fields) | field == old = new:fields
									| otherwise    = field:(replaceSheep old new fields)



-- hasWon function
hasWon :: Player -> Board -> Bool
hasWon Wolf board = getRow (getWolfPosition board) == 0
hasWon Sheep board = length (getMoves board Wolf) == 0



-- getResult function
getResult :: Board -> Player -> GameResult
getResult board player = if (hasWon player board)
						 then Victory
						 else if (hasWon (getPlayerOpponent player) board) 
						 	then Defeat
						 	else Unsettled



-- getScore function
getScore :: Board -> Player -> Int
getScore board Sheep = if (hasWon Sheep board)
					   then 14
					   else if (hasWon Wolf board)
							then -14
							else (getHeuristicScore board) - 13

getScore board Wolf = -(getScore board Sheep)


getHeuristicScore :: Board -> Int
getHeuristicScore board = (4 - (length (getMoves board Wolf))) * 3 +                               -- Wolf's moves factor
						  getRow (getWolfPosition board) +                                         -- Wolf's row factor
						  ((getSize board) - 1) - (minimum (map getRow (getSheepPositions board))) -- Sheep's minimum row factor
