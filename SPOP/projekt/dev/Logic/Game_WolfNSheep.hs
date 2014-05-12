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



-- getMoves function
getMoves :: Board -> Player -> [Move]
getMoves board Wolf = validateWolfMove wolfPosition (-1) (-1) boardSize sheepPositions ++
					  validateWolfMove wolfPosition (-1)   1  boardSize sheepPositions ++
					  validateWolfMove wolfPosition   1  (-1) boardSize sheepPositions ++
					  validateWolfMove wolfPosition   1    1  boardSize sheepPositions
					where
						boardSize = getSize board
						wolfPosition = getWolfPosition board
						wolfRow = getRow wolfPosition
						wolfColumn = getColumn wolfPosition
						wolfTopLeft = translate wolfPosition (-1) (-1)
						wolfTopRight = translate wolfPosition (-1) 1
						wolfBottomLeft = translate wolfPosition 1 (-1)
						wolfBottomRight = translate wolfPosition 1 1
						sheepPositions = getSheepPositions board

validateWolfMove :: Field -> Int -> Int -> Int -> [Field] -> [Move]
validateWolfMove position deltaRow deltaCol boardSize sheepPositions = if (destRow >= 0 && destRow < boardSize &&
																		   destCol >= 0 && destCol < boardSize &&
																		   not (elem destination sheepPositions))
																	   then [ MoveWolf (destination) ]
																	   else []
																		where
																			destination = translate position deltaRow deltaCol
																			destRow = getRow destination
																			destCol = getColumn destination



-- hasWon function
hasWon :: Player -> Board -> Bool
hasWon Wolf board = getRow (getWolfPosition board) == 0 -- || length (getMoves board Sheep) == 0
hasWon Sheep board = length (getMoves board Wolf) == 0
