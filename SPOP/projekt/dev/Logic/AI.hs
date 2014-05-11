module Logic.AI(aiMove) where

import Logic.Game
import Logic.GameTree

-- aiMove function
aiMove :: Board -> Player -> Board
aiMove currentBoard player = pick (getMoves currentBoard player) player

pick :: [Board] -> Player -> Board
pick [move] _ = move
pick (move1:move2:moves) player = if (getStateScore move1 (getPlayerOpponent player) player >= getStateScore move2 (getPlayerOpponent player) player)
								  then pick (move1:moves) player
								  else pick (move2:moves) player



aiMove1 :: Board -> Player -> Board
aiMove1 currentBoard player = getBoard (pickChild1 (getChildren (buildGameTree currentBoard player)))

pickChild1 :: [GameTree] => GameTree
pickChild1 [tree] = tree
pickChild1 (tree1:tree2:trees) = if (Logic.GameTree.getScore tree1 >= Logic.GameTree.getScore tree2)
								then pickChild1 (tree1 : trees)
								else pickChild1 (tree2 : trees)
