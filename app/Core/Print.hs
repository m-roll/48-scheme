module Core.Print (unwordsList) where

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show
