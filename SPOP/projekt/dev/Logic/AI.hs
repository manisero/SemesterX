module Logic.AI(aiMove) where

import Logic.Game
import Logic.GameTree

-------------------
-- Using Alpha-Beta
-------------------

-- aiMove function
aiMove :: Board -> Player -> Board
aiMove currentBoard player = pickChild (getMoves currentBoard player) player

pickChild :: [Board] -> Player -> Board
pickChild [move] _ = move
pickChild (move1:move2:moves) player = if (alphaBeta move1 (getPlayerOpponent player) player >= alphaBeta move2 (getPlayerOpponent player) player)
									   then pickChild (move1:moves) player
									   else pickChild (move2:moves) player

-----------------
-- Using minimax:
-----------------

-- aiMove function
aiMove1 :: Board -> Player -> Board
aiMove1 currentBoard player = pickChild1 (getMoves currentBoard player) player

pickChild1 :: [Board] -> Player -> Board
pickChild1 [move] _ = move
pickChild1 (move1:move2:moves) player = if (minimax move1 (getPlayerOpponent player) player >= minimax move2 (getPlayerOpponent player) player)
										then pickChild1 (move1:moves) player
										else pickChild1 (move2:moves) player



-------------------
-- Using game tree:
-------------------

aiMove2 :: Board -> Player -> Board
aiMove2 currentBoard player = getBoard (pickChild2 (getChildren (buildGameTree currentBoard player)))

pickChild2 :: [GameTree] => GameTree
pickChild2 [tree] = tree
pickChild2 (tree1:tree2:trees) = if (Logic.GameTree.getScore tree1 >= Logic.GameTree.getScore tree2)
								then pickChild2 (tree1 : trees)
								else pickChild2 (tree2 : trees)
