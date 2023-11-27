module Ast.Type (LispVal (Atom, List, DottedList, Number, String, Bool, Char, Float, Vector)) where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  | Float Float
  | Vector [LispVal] -- TODO: this should be constant time access, not a list.
