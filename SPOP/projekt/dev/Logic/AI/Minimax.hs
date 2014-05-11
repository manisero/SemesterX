module Logic.AI.Minimax(
	aiMove,
	minimax)
	where

import Logic.Game

-- aiMove function
aiMove :: Board -> Player -> Board
aiMove currentBoard player = pickChild (getMoves currentBoard player) player

pickChild :: [Board] -> Player -> Board
pickChild [move] _ = move
pickChild (move1:move2:moves) player = if (minimax move1 (getPlayerOpponent player) player >= minimax move2 (getPlayerOpponent player) player)
									   then pickChild (move1:moves) player
									   else pickChild (move2:moves) player



-- minimax function
minimax :: Board -> Player -> Player -> Int
minimax board currentPlayer rootPlayer = if (currentScore /= 0 || length moves == 0)
									     then currentScore
									     else if (currentPlayer == rootPlayer)
											then maximum movesScores
											else minimum movesScores
										 where
                                            currentScore = Logic.Game.getScore board rootPlayer
                                            moves = getMoves board currentPlayer
                                            movesScores = (map (\move -> minimax move (getPlayerOpponent currentPlayer) rootPlayer) moves)
