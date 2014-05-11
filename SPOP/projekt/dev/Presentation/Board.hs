module Presentation.Board(printBoard) where

import Data.List
import Logic.Game

-- printBoard function
printBoard :: Board -> String
printBoard board = intercalate "\n" [ show [ printField field | field <- row ] | row <- fields board ]

printField :: Field -> String
printField Empty = "-"
printField Cross = "X"
printField Circle = "O"