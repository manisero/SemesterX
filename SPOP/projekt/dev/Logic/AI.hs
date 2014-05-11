module Logic.AI(aiMove) where

import Logic.Game
import Logic.GameTree

-- aiMove function
aiMove :: Board -> Player -> Board
aiMove currentBoard player = board (pickChild (children (buildGameTree currentBoard player)))

pickChild :: [GameTree] => GameTree
pickChild [tree] = tree
pickChild (tree1:tree2:trees) = if (score tree1 >= score tree2)
								then pickChild (tree1 : trees)
								else pickChild (tree2 : trees)
