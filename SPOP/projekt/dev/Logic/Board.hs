module Logic.Board where

data Player = Crosses | Circles deriving Eq
data Field = Empty | Cross | Circle deriving Eq
	
instance Show Field where
    show Empty = "-"
    show Cross = "X"
    show Circle = "O"

class Board tBoard where
	fields :: tBoard -> [[Field]]
	size :: tBoard -> Int
	size board = length (fields board)
	field :: tBoard -> (Int, Int) -> Field
	field board (x, y) = ((fields board) !! y) !! x
	getScore :: tBoard -> Player -> Int
	getMoves :: tBoard -> Player -> [tBoard]
