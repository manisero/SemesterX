module Presentation.SaveLoad where

import Logic.TicTacToe

-- saveGame function
saveGame :: Board -> IO ()
saveGame board = do
					putStrLn ""
					putStrLn "Type file path:"
					putStrLn ""
					path <- getLine
					putStrLn ("Game saved to " ++ path)



-- loadGame function
loadGame :: IO Board
loadGame = do
			putStrLn ""
			putStrLn "Type file path:"
			putStrLn ""
			path <- getLine
			putStrLn ("Game loaded from " ++ path)
			return (Board [
						[Cross, Circle, Circle],
						[Cross, Empty, Circle],
						[Empty, Cross, Empty]
					])
