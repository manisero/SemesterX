module Logic.AI(aiMove) where

import Logic.Game
import Logic.GameTree

-- aiMove function
aiMove :: Board -> Player -> Board
aiMove currentBoard player = getBoard (pickChild (getChildren (buildGameTree currentBoard player)))

pickChild :: [GameTree] => GameTree
pickChild [tree] = tree
pickChild (tree1:tree2:trees) = if (Logic.GameTree.getScore tree1 >= Logic.GameTree.getScore tree2)
								then pickChild (tree1 : trees)
								else pickChild (tree2 : trees)
