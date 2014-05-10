module GameTree where
import Board

data GameTree = GameTree {
							board :: Board
							score :: Int
							children :: [GameTree]
						}
