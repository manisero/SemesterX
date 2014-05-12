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
getMoves board Wolf = (if (wolfRow > 0           && wolfColumn > 0           && not (elem wolfTopLeft     sheepPositions)) then [ MoveWolf (wolfTopLeft )    ] else []) ++
					  (if (wolfRow > 0           && wolfColumn < boardBorder && not (elem wolfTopRight    sheepPositions)) then [ MoveWolf (wolfTopRight)    ] else []) ++
					  (if (wolfRow < boardBorder && wolfColumn > 0           && not (elem wolfBottomLeft  sheepPositions)) then [ MoveWolf (wolfBottomLeft)  ] else []) ++
					  (if (wolfRow < boardBorder && wolfColumn < boardBorder && not (elem wolfBottomRight sheepPositions)) then [ MoveWolf (wolfBottomRight) ] else [])
					where
						boardBorder = (getSize board) - 1
						wolfPosition = getWolfPosition board
						wolfRow = getRow wolfPosition
						wolfColumn = getColumn wolfPosition
						wolfTopLeft = translate wolfPosition (-1) (-1)
						wolfTopRight = translate wolfPosition (-1) 1
						wolfBottomLeft = translate wolfPosition 1 (-1)
						wolfBottomRight = translate wolfPosition 1 1
						sheepPositions = getSheepPositions board



-- hasWon function
hasWon :: Player -> Board -> Bool
hasWon Wolf board = getRow (getWolfPosition board) == 0 -- || length (getMoves board Sheep) == 0
hasWon Sheep board = length (getMoves board Wolf) == 0
