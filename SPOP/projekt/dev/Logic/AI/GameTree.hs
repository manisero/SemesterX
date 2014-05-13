module Logic.AI.GameTree(
	aiMove,
	GameTree(getScore),buildGameTree)
	where

import Logic.Game

-- aiMove function
aiMove :: Board -> Player -> Board
aiMove currentBoard player = getBoard (pickChild (getChildren (buildGameTree currentBoard player)))

pickChild :: [GameTree] => GameTree
pickChild [tree] = tree
pickChild (tree1:tree2:trees) = if (Logic.AI.GameTree.getScore tree1 >= Logic.AI.GameTree.getScore tree2)
								then pickChild (tree1 : trees)
								else pickChild (tree2 : trees)



-- GameTree type
data GameTree = GameTree {
							getBoard :: Board,
							getScore :: Int,
							getChildren :: [GameTree]
						}
				deriving Show

-- buildGameTree function
buildGameTree :: Board -> Player -> GameTree
buildGameTree board player = buildGameTree' board (getMoves board player) player player

buildGameTree' :: Board -> [Board] -> Player ->  Player -> GameTree
buildGameTree' board [] rootPlayer _ = GameTree board (Logic.Game.getScore board rootPlayer) []
buildGameTree' board moves rootPlayer currentPlayer = GameTree board 
															   (if (rootScore /= 0) then rootScore else childrenScore)
															   (if (rootScore /= 0) then [] else children)
													  where
                                                          rootScore = Logic.Game.getScore board rootPlayer
                                                          opponent = getPlayerOpponent currentPlayer
                                                          children = [buildGameTree' move (getMoves move opponent) rootPlayer opponent | move <- moves]
                                                          childrenScore = getChildrenScore children rootPlayer currentPlayer

-- getChildrenScore function
getChildrenScore :: [GameTree] -> Player -> Player -> Int
getChildrenScore children rootPlayer currentPlayer = if (currentPlayer == rootPlayer)
													 then maximum scores
													 else minimum scores
													 where scores = map Logic.AI.GameTree.getScore children
