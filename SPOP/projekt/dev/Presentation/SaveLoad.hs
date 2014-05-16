module Presentation.SaveLoad(
	saveGame,
	loadGame)
	where

import System.IO.Error
import Control.Exception
import Logic.Game

-- saveGame function
saveGame :: Board -> IO Bool
saveGame board = do
					putStrLn ""
					putStrLn "Type file path:"
					path <- getLine
					catch (do
							writeFile path (show board)
							putStrLn ""
							putStrLn ("Game saved to: " ++ path)
							return True)
						  handleSaveError


-- handleSaveError function
handleSaveError :: IOError -> IO Bool
handleSaveError e = do
						putStrLn ""
						putStrLn ("Error: " ++ ioeGetErrorString e)
						return False



-- loadGame function
loadGame :: IO (Maybe Board)
loadGame = do
			putStrLn ""
			putStrLn "Type file path:"
			path <- getLine
			content <- readFile path
			putStrLn ""
			putStrLn ("Game loaded from: " ++ path)
			return (readBoard content)

readBoard :: String -> Maybe Board
readBoard string = case reads string of
					[(board, "")] -> Just board
					_         -> Nothing
