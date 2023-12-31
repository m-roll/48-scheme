module Eval.Primitives (primitives) where

import Control.Monad.Except
import Control.Monad.Identity
import Data.Function (on)
import Data.Functor
import Data.Text (pack, toLower, unpack)
import Eval.Coercion
import Eval.Equality
import Eval.LispError
import Eval.LispVal
import Eval.ThrowsError

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    ("<=", numBoolBinop (<=)),
    (">=", numBoolBinop (>=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("make-string", makeString),
    ("string", stringFromChars),
    ("string-length", stringLength),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("string-ci=?", strBoolBinopCaseInsensitive (==)),
    ("string-ci<?", strBoolBinopCaseInsensitive (<)),
    ("string-ci>?", strBoolBinopCaseInsensitive (>)),
    ("string-ci<=?", strBoolBinopCaseInsensitive (<=)),
    ("string-ci>=?", strBoolBinopCaseInsensitive (>=)),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("boolean?", return . checkBool),
    ("list?", return . checkList),
    ("symbol?", return . checkSymbol),
    ("char?", return . checkChar),
    ("zero?", return . checkZero),
    ("vector?", return . checkVector),
    ("symbol->string", return . symbolToString),
    ("string->symbol", return . stringToSymbol),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eqv?", eqv),
    ("eq?", eqv),
    ("equal?", equal)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params <&> Number . foldl1 op

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [arg1, arg2] = do
  left <- unpacker arg1
  right <- unpacker arg2
  return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

makeString :: [LispVal] -> ThrowsError LispVal
makeString [arg1, arg2] = do
  n <- unpackNum arg1
  c <- unpackChar arg2
  return . String $ replicate (fromIntegral n) c
makeString [arg1] = do
  n <- unpackNum arg1
  return . String $ replicate (fromIntegral n) '0'
makeString badArgList = throwError $ NumArgs 2 badArgList

stringFromChars :: [LispVal] -> ThrowsError LispVal
stringFromChars args = do
  strResult <- mapM unpackChar args
  return $ String strResult

strBoolBinopCaseInsensitive :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinopCaseInsensitive f = strBoolBinop (f `on` strToLower)
  where
    strToLower = unpack . toLower . pack

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [arg] = return . Number . toInteger . length $ unpackStr arg
stringLength badArgList = throwError $ NumArgs 1 badArgList

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError . TypeMismatch "pair" $ badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [] x] = return x
cdr [DottedList (_ : xs) r] = return $ DottedList xs r
cdr [badArg] = throwError . TypeMismatch "pair" $ badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
-- The book also has a case specifically for when the second arg is (), but that's redundant since it's identical to this
cons [x, List xs] = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError . NumArgs 2 $ badArgList
