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



-- Move type
data Move = MoveWolf Field | MoveSheep Field Field



-- Board type
data Board = Board {
					getSize :: Int,
					getWolfPosition :: Field,
					getSheepPositions :: [Field]
				   } deriving (Show, Read)



-- hasWon function
hasWon :: Player -> Board -> Bool
hasWon Wolf board = getRow (getWolfPosition board) == 0 -- || length (getMoves board Sheep) == 0
hasWon Sheep board = length (getMoves board Wolf) == 0



-- getMoves function
getMoves :: Board -> Player -> [Move]
getMoves _ _ = []