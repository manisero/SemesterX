module Presentation.SaveLoad(
	saveGame,
	loadGame)
	where

import Logic.Game

-- saveGame function
saveGame :: Board -> IO ()
saveGame board = do
					putStrLn ""
					putStrLn "Type file path:"
					path <- getLine
					writeFile path (show board)
					putStrLn ""
					putStrLn ("Game saved to: " ++ path)



-- loadGame function
loadGame :: IO Board
loadGame = do
			putStrLn ""
			putStrLn "Type file path:"
			path <- getLine
			content <- readFile path
			putStrLn ""
			putStrLn ("Game loaded from: " ++ path)
			return (read content::Board)
