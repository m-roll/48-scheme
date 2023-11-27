module Ast.Print (showVal, unwordsList) where

import Ast.Type

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List l) = "(" ++ unwordsList l ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ showVal tail' ++ ")"
showVal (Char c) = "#\\" ++ [c]
showVal (Vector v) = "#(" ++ unwordsList v ++ ")"
showVal (Float f) = show f

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
