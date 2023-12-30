module Eval.LispVal.Instance (Show) where

import Eval.Type

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
showVal (PrimitiveFunc _) = "#prim"
showVal (Func {}) = "func"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
