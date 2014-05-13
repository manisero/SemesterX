module Logic.AI.Heuristic(
	aiMove,
	alphaBetaHeuristic)
	where

import Logic.Game_WolfNSheep

-- alphaBetaHeuristicDepth constant
alphaBetaHeuristicDepth :: Int
alphaBetaHeuristicDepth = 8



-- getChildren function
getChildren :: Board -> Player -> [Board]
getChildren board player = map (\move -> applyMove move board) (getMoves board player)



-- aiMove function
aiMove :: Board -> Player -> Board
aiMove currentBoard player = pickChild (getChildren currentBoard player) player alphaBetaHeuristicDepth

pickChild :: [Board] -> Player -> Int -> Board
pickChild [move] _ _ = move
pickChild (move1:move2:moves) player depth = if (alphaBetaHeuristic move1 (getPlayerOpponent player) player depth >= alphaBetaHeuristic move2 (getPlayerOpponent player) player depth)
											 then pickChild (move1:moves) player depth
											 else pickChild (move2:moves) player depth



-- alphaBetaHeuristic function
alphaBetaHeuristic :: Board -> Player -> Player -> Int -> Int
alphaBetaHeuristic board currentPlayer rootPlayer depth = alphaBetaHeuristic' board currentPlayer rootPlayer (minBound::Int) (maxBound::Int) depth

alphaBetaHeuristic' :: Board -> Player -> Player -> Int -> Int -> Int -> Int
alphaBetaHeuristic' board currentPlayer rootPlayer alpha beta depth = if (depth == 0 || length moves == 0)
																	  then Logic.Game_WolfNSheep.getScore board rootPlayer
																	  else if (currentPlayer == rootPlayer)
																		then alphaLoop moves currentPlayer rootPlayer alpha beta depth
																		else betaLoop moves currentPlayer rootPlayer alpha beta depth
																		where
																			moves = getChildren board currentPlayer

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
