module Logic.Game(
	GameResult(Unsettled, Victory, Defeat),
	Player(Wolf, Sheep), getPlayerOpponent,
	Field, getRow, getColumn, translate, topLeftOf, topRightOf, bottomLeftOf, bottomRightOf,
	Move(MoveWolf, MoveSheep),
	Board(Board), getSize, getWolfPosition, getSheepPositions, isValid,
	isMoveAllowed, getMoves, applyMove,
	hasWon, getResult, getScore)
	where

-- Reference: http://pl.wikipedia.org/wiki/Wilk_i_owce

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
data Move = MoveWolf Field        -- move Wolf to ...
		  | MoveSheep Field Field -- move Sheep from ... to ...
			deriving (Show)



-- Board type
data Board = Board {
					getSize :: Int,
					getWolfPosition :: Field,
					getSheepPositions :: [Field]
				   } deriving (Show, Read)



-- isValid function
isValid :: Board -> Bool
isValid (Board size wolfPosition sheepPositions) = all checkPosition allPositions &&
												   length [ True | pos1 <- allPositions, pos2 <- allPositions, pos1 == pos2 ] == length allPositions -- check for position collisions
													where
														checkPosition (row, col) = row >= 0 && row < size && col >= 0 && col < size && -- fits in board
																				   odd (row + col)                                     -- is on black field
														allPositions = wolfPosition : sheepPositions



-- isMoveAllowed
isMoveAllowed :: Move -> Board -> Bool
isMoveAllowed (MoveWolf (row, col)) board = (row == wolfRow - 1 || row == wolfRow + 1) &&                 -- is one row forward or backward
											(col == wolfCol - 1 || col == wolfCol + 1) &&                 -- is one column to the left or right
											row >= 0 && row < boardSize && col >= 0 && col < boardSize && -- fits in board
											not (elem (row, col) (getSheepPositions board))               -- does not collide with any Sheep
												where
													wolfPosition = getWolfPosition board
													wolfRow = getRow wolfPosition
													wolfCol = getColumn wolfPosition
													boardSize = getSize board

isMoveAllowed (MoveSheep (fromRow, fromCol) (toRow, toCol)) board = toRow == fromRow + 1 &&                                 -- is one row forward
																	(toCol == fromCol - 1 || toCol == fromCol + 1) &&       -- is one column to the left or right
																	toRow < boardSize && toCol >= 0 && toCol < boardSize && -- fits in board
																	destination /= (getWolfPosition board) &&               -- does not collide with Wolf
													 				not (elem destination (getSheepPositions board))        -- does not collide with any other Sheep
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
-- Calculates score of the current state of the board. Range: from -14 to 14.
getScore :: Board -> Player -> Int
getScore board Sheep = if (hasWon Sheep board)
					   then 14
					   else if (hasWon Wolf board)
							then -14
							else (getHeuristicScore board) - 13 -- range: from -13 to 13

getScore board Wolf = -(getScore board Sheep)


-- Calculates heuristic score of the current state of the board. Range: from 0 to 26.
getHeuristicScore :: Board -> Int
getHeuristicScore board = (4 - (length (getMoves board Wolf))) * 3 +                               -- Possible Wolf's moves factor (0 moves: 12, 1 move: 9 etc.)
						  getRow (getWolfPosition board) +                                         -- Wolf's row factor (from 0 to 7)
						  ((getSize board) - 1) - (minimum (map getRow (getSheepPositions board))) -- Sheep's minimum row factor: (from 0 to 7)
