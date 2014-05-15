module Logic.AI.Heuristic(
	aiMove, aiMove_customDepth,
	alphaBetaHeuristic)
	where

import Logic.Game_WolfNSheep

-- defaultAlphaBetaHeuristicDepth constant
defaultAlphaBetaHeuristicDepth :: Int
defaultAlphaBetaHeuristicDepth = 11



-- aiMove function
aiMove :: Board -> Player -> Maybe Board
aiMove currentBoard player = aiMove_customDepth currentBoard player defaultAlphaBetaHeuristicDepth

aiMove_customDepth :: Board -> Player -> Int -> Maybe Board
aiMove_customDepth currentBoard player depth = pickChild (getChildren currentBoard player) player depth

pickChild :: [Board] -> Player -> Int -> Maybe Board
pickChild [] _ _ = Nothing
pickChild [child] _ _ = Just child
pickChild (child1:child2:children) player depth = if (alphaBetaHeuristic child1 (getPlayerOpponent player) player depth >= alphaBetaHeuristic child2 (getPlayerOpponent player) player depth)
												  then pickChild (child1:children) player depth
												  else pickChild (child2:children) player depth



-- getChildren function
getChildren :: Board -> Player -> [Board]
getChildren board player = map (\move -> applyMove move board) (getMoves board player)



-- alphaBetaHeuristic function
alphaBetaHeuristic :: Board -> Player -> Player -> Int -> Int
alphaBetaHeuristic board currentPlayer rootPlayer depth = alphaBetaHeuristic' board currentPlayer rootPlayer (minBound::Int) (maxBound::Int) depth

alphaBetaHeuristic' :: Board -> Player -> Player -> Int -> Int -> Int -> Int
alphaBetaHeuristic' board currentPlayer rootPlayer alpha beta depth = if (depth == 0 || getResult board rootPlayer /= Unsettled)
																	  then getScore board rootPlayer
																	  else if (currentPlayer == rootPlayer)
																		then alphaLoop children currentPlayer rootPlayer alpha beta depth
																		else betaLoop children currentPlayer rootPlayer alpha beta depth
																		where
																			children = getChildren board currentPlayer


alphaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int -> Int
alphaLoop [] _ _ alpha _ _ = alpha
alphaLoop (child:children) currentPlayer rootPlayer alpha beta depth = if (newAlpha >= beta)
																	   then newAlpha
																	   else alphaLoop children currentPlayer rootPlayer newAlpha beta depth
																		where newAlpha = max alpha (alphaBetaHeuristic' child (getPlayerOpponent currentPlayer) rootPlayer alpha beta (depth - 1))

betaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int -> Int
betaLoop [] _ _ _ beta _ = beta
betaLoop (child:children) currentPlayer rootPlayer alpha beta depth = if (newBeta <= alpha)
																	  then newBeta
																	  else betaLoop children currentPlayer rootPlayer alpha newBeta depth
																		where newBeta = min beta (alphaBetaHeuristic' child (getPlayerOpponent currentPlayer) rootPlayer alpha beta (depth - 1))
