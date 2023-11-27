module Eval where

import Ast
import Control.Monad.Except ()
import Eval.LispError ()

-- TODO move error into own module hierarchy

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Float _) = val
eval val@(Char _) = val
eval val@(Atom _) = val
eval val@(Vector _) = val
eval val@(DottedList _ _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(List _) = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("boolean?", checkBool),
    ("list?", checkList),
    ("symbol?", checkSymbol),
    ("char?", checkChar),
    ("zero?", checkZero),
    ("vector?", checkVector),
    ("symbol->string", symbolToString),
    ("string->symbol", stringToSymbol)
  ]

checkVector :: [LispVal] -> LispVal
checkVector (Vector _ : _) = Bool True
checkVector _ = Bool False

checkZero :: [LispVal] -> LispVal
checkZero (Number 0 : _) = Bool True
checkZero _ = Bool False

checkChar :: [LispVal] -> LispVal
checkChar (Char _ : _) = Bool True
checkChar _ = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString (Atom n : _) = String n
symbolToString _ = Bool False

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol (String n : _) = Atom n
stringToSymbol _ = Bool False

checkList :: [LispVal] -> LispVal
checkList (List _ : _) = Bool True
checkList _ = Bool False

checkSymbol :: [LispVal] -> LispVal
checkSymbol (Atom _ : _) = Bool True
checkSymbol _ = Bool False

checkBool :: [LispVal] -> LispVal
checkBool (Bool _ : _) = Bool True
checkBool _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
