module Logic.GameTree where
import Logic.Board
import Logic.TicTacToe

data GameTree = GameTree {
							board :: TTTBoard,
							score :: Int,
							children :: [GameTree]
						}
				deriving Show

buildGameTree :: TTTBoard -> Player -> GameTree
buildGameTree board player = buildGameTree' board (getMoves board player) player player

buildGameTree' :: TTTBoard -> [TTTBoard] -> Player ->  Player -> GameTree
buildGameTree' board [] rootPlayer _ = GameTree board (getScore board rootPlayer) []
buildGameTree' board moves rootPlayer currentPlayer = GameTree board 
															   (if (rootScore /= 0) then rootScore else childrenScore)
															   (if (rootScore /= 0) then [] else children)
													  where
                                                          rootScore = getScore board rootPlayer
                                                          opponent = playerOpponent currentPlayer
                                                          children = [buildGameTree' move (getMoves move opponent) rootPlayer opponent | move <- moves]
                                                          childrenScore = getChildrenScore children rootPlayer currentPlayer

getChildrenScore :: [GameTree] -> Player -> Player -> Int
getChildrenScore children rootPlayer currentPlayer = if (currentPlayer == rootPlayer)
													 then maximum [score child | child <- children]
													 else minimum [score child | child <- children]