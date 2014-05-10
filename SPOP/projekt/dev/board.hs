module Board where

data Player = Crosses | Circles

data Field = Empty | Cross | Circle deriving (Eq, Show)
data Board = Board { fields :: [[Field]] }

type Movement = (Int, Int)

size :: Board -> Int
size board = length (fields board)

field :: Board -> Int -> Int -> Field
field board x y = ((fields board) !! y) !! x

moves :: Board -> Player -> [Movement]
moves board player = [(x, y) | x <- [1 .. (size board)], y <- [1 .. (size board)], field board x y == Empty]