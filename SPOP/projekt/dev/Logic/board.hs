module Logic.Board where

data Player = Crosses | Circles
data Field = Empty | Cross | Circle deriving (Eq, Show)
type Move = (Int, Int)

class Board tBoard where
	fields :: tBoard -> [[Field]]
	size :: tBoard -> Int
	size board = length (fields board)
	field :: tBoard -> (Int, Int) -> Field
	field board (x, y) = ((fields board) !! y) !! x
	score :: tBoard -> Player -> Int
	moves :: tBoard -> Player -> [Move]
