module Presentation.SaveLoad(
	saveGame,
	loadGame)
	where

import System.IO.Error
import Control.Exception
import Data.Maybe
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



-- loadGame function
loadGame :: IO (Maybe Board)
loadGame = do
			putStrLn ""
			putStrLn "Type file path:"
			path <- getLine
			content <- catch (do
								fileContent <- readFile path
								return (Just fileContent))
							 handleLoadError
			if (isJust content)
				then do
					let board = readBoard (fromJust content)
					if (isJust board)
						then do
							putStrLn ""
							putStrLn ("Game loaded from: " ++ path)
							return board
						else handleError "Invalid file content" Nothing
				else return Nothing


readBoard :: String -> Maybe Board
readBoard string = case reads string of
					[(board, "")] -> Just board
					_         -> Nothing



-- handleError function
handleError :: String -> tResult -> IO tResult
handleError errorMessage result = do
								putStrLn ""
								putStrLn ("Error: " ++ errorMessage)
								return result


handleSaveError :: IOError -> IO Bool
handleSaveError e = handleError (ioeGetErrorString e) False


handleLoadError :: IOError -> IO (Maybe string)
handleLoadError e = handleError (ioeGetErrorString e) Nothing
