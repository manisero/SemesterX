module Logic.GameTree where
import Logic.Board

data GameTree = GameTree {
							board :: Board
							score :: Int
							children :: [GameTree]
						}
