module Logic.AI.Heuristic where

import Logic.Game



-- alphaBetaHeuristic function
alphaBetaHeuristic :: Board -> Player -> Player -> Int -> Int
alphaBetaHeuristic board currentPlayer rootPlayer depth = alphaBetaHeuristic' board currentPlayer rootPlayer (minBound::Int) (maxBound::Int) depth

alphaBetaHeuristic' :: Board -> Player -> Player -> Int -> Int -> Int -> Int
alphaBetaHeuristic' board currentPlayer rootPlayer alpha beta depth = if (depth == 0 || length moves == 0)
																	  then Logic.Game.getScore board rootPlayer
																	  else if (currentPlayer == rootPlayer)
																		then alphaLoop moves currentPlayer rootPlayer alpha beta depth
																		else betaLoop moves currentPlayer rootPlayer alpha beta depth
																		where
																			moves = getMoves board currentPlayer

alphaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int -> Int
alphaLoop [] _ _ alpha _ _ = alpha
alphaLoop (move:moves) currentPlayer rootPlayer alpha beta depth = if (newAlpha >= beta)
																   then newAlpha
																   else alphaLoop moves currentPlayer rootPlayer newAlpha beta depth
																	where newAlpha = max alpha (alphaBetaHeuristic' move (getPlayerOpponent currentPlayer) rootPlayer alpha beta (depth - 1))

betaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int -> Int
betaLoop [] _ _ _ beta _ = beta
betaLoop (move:moves) currentPlayer rootPlayer alpha beta depth = if (newBeta <= alpha)
																  then newBeta
																  else betaLoop moves currentPlayer rootPlayer alpha newBeta depth
																	where newBeta = min beta (alphaBetaHeuristic' move (getPlayerOpponent currentPlayer) rootPlayer alpha beta (depth - 1))
