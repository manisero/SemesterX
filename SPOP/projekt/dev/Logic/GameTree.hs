module Logic.GameTree(
	GameTree(GameTree,getBoard,getScore,getChildren),
	buildGameTree, getStateScore, alphaBeta)
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
                                                    movesScores = (map (\move -> getStateScore move (getPlayerOpponent currentPlayer) rootPlayer) moves)



------------------------
-- Alhpa-Beta algorithm:
------------------------

alphaBeta :: Board -> Player -> Player -> Int -> Int -> Int
alphaBeta board currentPlayer rootPlayer alpha beta = if (currentScore /= 0 || length moves == 0)
													     then currentScore
													     else if (currentPlayer == rootPlayer)
															then alphaLoop moves currentPlayer rootPlayer alpha beta
															else betaLoop moves currentPlayer rootPlayer alpha beta
															where
			                                                    currentScore = Logic.Game.getScore board rootPlayer
			                                                    moves = getMoves board currentPlayer

alphaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int
alphaLoop [] _ _ alpha _ = alpha
alphaLoop (move:moves) currentPlayer rootPlayer alpha beta = if (newAlpha >= beta)
															 then newAlpha
															 else alphaLoop moves currentPlayer rootPlayer newAlpha beta
																where newAlpha = max alpha (alphaBeta move (getPlayerOpponent currentPlayer) rootPlayer alpha beta)

betaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int
betaLoop [] _ _ _ beta = beta
betaLoop (move:moves) currentPlayer rootPlayer alpha beta = if (newBeta <= alpha)
															then newBeta
															else betaLoop moves currentPlayer rootPlayer alpha newBeta
																where newBeta = min beta (alphaBeta move (getPlayerOpponent currentPlayer) rootPlayer alpha beta)
