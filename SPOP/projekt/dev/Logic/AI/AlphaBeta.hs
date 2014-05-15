module Logic.AI.AlphaBeta(
	aiMove,
	alphaBeta)
	where

import Logic.Game_TicTacToe

-- aiMove function
aiMove :: Board -> Player -> Board
aiMove currentBoard player = pickChild (getMoves currentBoard player) player

pickChild :: [Board] -> Player -> Board
pickChild [move] _ = move
pickChild (move1:move2:moves) player = if (alphaBeta move1 (getPlayerOpponent player) player >= alphaBeta move2 (getPlayerOpponent player) player)
									   then pickChild (move1:moves) player
									   else pickChild (move2:moves) player



-- alphaBeta function
alphaBeta :: Board -> Player -> Player -> Int
alphaBeta board currentPlayer rootPlayer = alphaBeta' board currentPlayer rootPlayer (minBound::Int) (maxBound::Int)

alphaBeta' :: Board -> Player -> Player -> Int -> Int -> Int
alphaBeta' board currentPlayer rootPlayer alpha beta = if (length moves == 0)
													     then getScore board rootPlayer
													     else if (currentPlayer == rootPlayer)
															then alphaLoop moves currentPlayer rootPlayer alpha beta
															else betaLoop moves currentPlayer rootPlayer alpha beta
															where
			                                                    moves = getMoves board currentPlayer

alphaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int
alphaLoop [] _ _ alpha _ = alpha
alphaLoop (move:moves) currentPlayer rootPlayer alpha beta = if (newAlpha >= beta)
															 then newAlpha
															 else alphaLoop moves currentPlayer rootPlayer newAlpha beta
																where newAlpha = max alpha (alphaBeta' move (getPlayerOpponent currentPlayer) rootPlayer alpha beta)

betaLoop :: [Board] -> Player -> Player -> Int -> Int -> Int
betaLoop [] _ _ _ beta = beta
betaLoop (move:moves) currentPlayer rootPlayer alpha beta = if (newBeta <= alpha)
															then newBeta
															else betaLoop moves currentPlayer rootPlayer alpha newBeta
																where newBeta = min beta (alphaBeta' move (getPlayerOpponent currentPlayer) rootPlayer alpha beta)
