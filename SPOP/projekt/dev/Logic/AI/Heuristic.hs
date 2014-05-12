module Logic.AI.Heuristic where

import Logic.Game



-- alphaBetaHeuristic function
alphaBetaHeuristic :: Board -> Player -> Player -> Int
alphaBetaHeuristic board currentPlayer rootPlayer = alphaBetaHeuristic' board currentPlayer rootPlayer (minBound::Int) (maxBound::Int)

alphaBetaHeuristic' :: Board -> Player -> Player -> Int -> Int -> Int
alphaBetaHeuristic' board currentPlayer rootPlayer alpha beta = if (currentScore /= 0 || length moves == 0)
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
																where newAlpha = max alpha (alphaBetaHeuristic' move (getPlayerOpponent currentPlayer) rootPlayer alpha beta)

betaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int
betaLoop [] _ _ _ beta = beta
betaLoop (move:moves) currentPlayer rootPlayer alpha beta = if (newBeta <= alpha)
															then newBeta
															else betaLoop moves currentPlayer rootPlayer alpha newBeta
																where newBeta = min beta (alphaBetaHeuristic' move (getPlayerOpponent currentPlayer) rootPlayer alpha beta)
