module Logic.Game_WolfNSheep where

-- GameResult type
data GameResult = Unsettled | Victory | Defeat | Draw deriving (Eq, Show)



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
isMoveAllowed (MoveWolf (row, column)) board = row    >= 0 && row    < boardSize &&
											   column >= 0 && column < boardSize &&
											   not (elem (row, column) (getSheepPositions board))
											   where boardSize = getSize board



-- getMoves function
getMoves :: Board -> Player -> [Move]
getMoves board Wolf = (if (isMoveAllowed topLeft     board) then [ topLeft ] else []) ++
					  (if (isMoveAllowed topRight    board) then [ topRight ] else []) ++
					  (if (isMoveAllowed bottomLeft  board) then [ bottomLeft ] else []) ++
					  (if (isMoveAllowed bottomRight board) then [ bottomRight ] else [])
						where
							wolfPosition = getWolfPosition board
							topLeft = MoveWolf (translate wolfPosition (-1) (-1))
							topRight = MoveWolf (translate wolfPosition (-1) 1)
							bottomLeft = MoveWolf (translate wolfPosition 1 (-1))
							bottomRight = MoveWolf (translate wolfPosition 1 1)



-- hasWon function
hasWon :: Player -> Board -> Bool
hasWon Wolf board = getRow (getWolfPosition board) == 0 -- || length (getMoves board Sheep) == 0
hasWon Sheep board = length (getMoves board Wolf) == 0
