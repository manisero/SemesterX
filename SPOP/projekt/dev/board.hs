module Board where

-- generic classes
class Board tBoard where
	fields :: tBoard -> [[Field]]
	size :: tBoard -> Int
	size board = length (fields board)
	field :: tBoard -> Int -> Int -> Field
	field board x y = ((fields board) !! y) !! x
	moves :: Board tBoard => tBoard -> Player -> [Move]

-- tic-tac-toe types
data Player = Crosses | Circles
data Field = Empty | Cross | Circle deriving (Eq, Show)
type Move = (Int, Int)

data TTTBoard = TTTBoard [[Field]]
instance Board TTTBoard where
	fields (TTTBoard fs) = fs
	moves board player = [(x, y) | x <- [0 .. ((size board) - 1)], y <- [0 .. ((size board) - 1)], field board x y == Empty]
