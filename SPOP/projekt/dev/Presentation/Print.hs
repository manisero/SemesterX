module Presentation.Print(printBoard) where

import Data.List
import Logic.Game

-- printBoard function
printBoard :: Board -> IO ()
printBoard board = putStrLn (intercalate "\n" [ show [ showField field | field <- row ] | row <- getFields board ])

showField :: Field -> String
showField Empty = "-"
showField Cross = "X"
showField Circle = "O"
