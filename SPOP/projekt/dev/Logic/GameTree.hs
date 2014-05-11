module Logic.GameTree(
	GameTree(GameTree,getBoard,getScore,getChildren),
	buildGameTree, getStateScore, minimaxAplha)
	where

import Logic.Game

---------------------------
-- Explicit game tree usge:
---------------------------

-- GameTree type
data GameTree = GameTree {
							getBoard :: Board,
							getScore :: Int,
							getChildren :: [GameTree]
						}
				deriving Show

-- buildGameTree function
buildGameTree :: Board -> Player -> GameTree
buildGameTree board player = buildGameTree' board (getMoves board player) player player

buildGameTree' :: Board -> [Board] -> Player ->  Player -> GameTree
buildGameTree' board [] rootPlayer _ = GameTree board (Logic.Game.getScore board rootPlayer) []
buildGameTree' board moves rootPlayer currentPlayer = GameTree board 
															   (if (rootScore /= 0) then rootScore else childrenScore)
															   (if (rootScore /= 0) then [] else children)
													  where
                                                          rootScore = Logic.Game.getScore board rootPlayer
                                                          opponent = getPlayerOpponent currentPlayer
                                                          children = [buildGameTree' move (getMoves move opponent) rootPlayer opponent | move <- moves]
                                                          childrenScore = getChildrenScore children rootPlayer currentPlayer

getChildrenScore :: [GameTree] -> Player -> Player -> Int
getChildrenScore children rootPlayer currentPlayer = if (currentPlayer == rootPlayer)
													 then maximum scores
													 else minimum scores
													 where scores = map Logic.GameTree.getScore children




--------------------------------
-- Recursive game tree scanning:
--------------------------------

-- getStateScore function
getStateScore :: Board -> Player -> Player -> Int
getStateScore board currentPlayer rootPlayer = if (currentScore /= 0 || length moves == 0)
											   then currentScore
											   else if (currentPlayer == rootPlayer)
													then maximum movesScores
													else minimum movesScores
												where
                                                    currentScore = Logic.Game.getScore board rootPlayer
                                                    moves = getMoves board currentPlayer
                                                    movesScores = map (\move -> getStateScore move (getPlayerOpponent currentPlayer) rootPlayer) moves



------------------------
-- Alhpa-Beta algorithm:
------------------------

minimaxAplha :: Board -> Player -> Player -> Int -> Int -> Int
minimaxAplha board currentPlayer rootPlayer alpha beta = if (currentScore /= 0 || length moves == 0)
													     then currentScore
													     else if (currentPlayer == rootPlayer)
															then maximum (alpha:movesScores) --if (maximum (alpha:movesScores) >= beta)
																 --then alpha
																 --else beta
															else minimum (beta:movesScores) --if (minimum (beta:movesScores) <= alpha)
																 --then beta
																 --else alpha
															where
			                                                    currentScore = Logic.Game.getScore board rootPlayer
			                                                    moves = getMoves board currentPlayer
			                                                    movesScores = map (\move -> minimaxAplha move (getPlayerOpponent currentPlayer) rootPlayer alpha beta) moves
