module Eval where

import Ast
import Control.Monad ()
import Control.Monad.Except (throwError)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.Text (pack, toLower, unpack)
import Eval.Coercion (Unpacker (AnyUnpacker), unpackBool, unpackChar, unpackEquals, unpackNum, unpackStr)
import Eval.Env
import Eval.LispError (LispError (BadSpecialForm, CaseNotMatched, NotFunction, NumArgs, TypeMismatch), ThrowsError)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Float _) = return val
eval _ val@(Char _) = return val
eval _ val@(Atom _) = return val
eval _ val@(Vector _) = return val
eval _ val@(DottedList _ _) = return val
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred', conseq, alt]) = ifSpecialForm env pred' conseq alt
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval _ badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

ifSpecialForm :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
ifSpecialForm env pred' conseq alt = do
  result <- eval env pred'
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    badArg -> throwError $ TypeMismatch "bool" badArg

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

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
    ("equal?", equal),
    ("case", caseP),
    ("cond", condP)
  ]

makeString :: [LispVal] -> ThrowsError LispVal
makeString [arg1, arg2] = do
  n <- unpackNum arg1
  c <- unpackChar arg2
  return . String $ replicate (fromIntegral n) c
makeString [arg1] = do
  n <- unpackNum arg1
  return . String $ replicate (fromIntegral n) '0'
makeString badArgList = throwError $ NumArgs 2 badArgList

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [arg] = return . Number . toInteger . length $ unpackStr arg
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringFromChars :: [LispVal] -> ThrowsError LispVal
stringFromChars args = do
  strResult <- mapM unpackChar args
  return $ String strResult

-- TODO implement `else`
condP :: [LispVal] -> Env -> ThrowsError LispVal
condP env = evalCaseP
  where
    evalCaseP :: [LispVal] -> ThrowsError LispVal
    evalCaseP (List [pred', conseq] : rest) = do
      result <- eval env pred'
      case result of
        Bool True -> eval env conseq
        Bool False -> evalCaseP rest
        badArg -> throwError $ TypeMismatch "bool" badArg
    evalCaseP (badForm : _) = throwError $ BadSpecialForm "unrecognized case form" badForm
    evalCaseP [] = throwError CaseNotMatched

caseP :: [LispVal] -> ThrowsError LispVal
caseP (key : allClauses) = do
  keyResult <- eval key
  let evalExprSeq = undefined
  let checkMatch = foldr ((||) . eqvHelper keyResult) False
  let evalClauses clauses = case clauses of
        [List (Atom "else" : exprs)] -> evalExprSeq exprs
        ((List ((List datap) : exprs)) : restClauses) ->
          if checkMatch datap
            then evalExprSeq exprs
            else evalClauses restClauses
        (badForm : _) -> throwError $ BadSpecialForm "expected else or case clause" badForm
        [] -> throwError CaseNotMatched
  evalClauses allClauses
caseP [] = undefined

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [] x] = return x
cdr [DottedList (_ : xs) r] = return $ DottedList xs r
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
-- The book also has a case specifically for when the second arg is (), but that's redundant since it's identical to this
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- I dislike the implementation in the book. This is my own version.
-- This could also be the impl for the haskell Eq instance
eqv :: [LispVal] -> ThrowsError LispVal
eqv [arg1, arg2] = return $ Bool $ eqvHelper arg1 arg2
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvHelper :: LispVal -> LispVal -> Bool
eqvHelper (Bool arg1') (Bool arg2') = arg1' == arg2'
eqvHelper (Number arg1') (Number arg2') = arg1' == arg2'
eqvHelper (String arg1') (String arg2') = arg1' == arg2'
eqvHelper (Atom arg1') (Atom arg2') = arg1' == arg2'
eqvHelper (DottedList xs x) (DottedList ys y) = eqvList (xs ++ [x]) (ys ++ [y])
eqvHelper (List xs) (List ys) = eqvList xs ys
eqvHelper _ _ = False

eqvList :: [LispVal] -> [LispVal] -> Bool
eqvList xs ys = runIdentity $ listEquality eqvHelperM xs ys
  where
    eqvHelperM x y = Identity $ eqvHelper x y

-- This could probably be expressed more idiomatically.
-- The recursive part is fine, but there's no reason to only allow monad inputs here.
-- Perhaps some combination of lifting could help here.
listEquality :: Monad m => (LispVal -> LispVal -> m Bool) -> [LispVal] -> [LispVal] -> m Bool
listEquality eqChecker (x : xs) (y : ys) = do
  pairEq <- eqChecker x y
  reqEq <- listEquality eqChecker xs ys
  return $ pairEq && reqEq
listEquality _ [] [] = return True
listEquality _ _ _ = return False

equal :: [LispVal] -> ThrowsError LispVal
equal [List xs, List ys] = fmap Bool (listEquality equalHelper xs ys)
equal [arg1, arg2] = fmap Bool (equalHelper arg1 arg2)
equal badArgList = throwError $ NumArgs 2 badArgList

equalHelper :: LispVal -> LispVal -> ThrowsError Bool
equalHelper arg1 arg2 = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return (primitiveEquals || let (Bool x) = eqvEquals in x) -- I don't like this non-exhaustive pattern matching

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [arg1, arg2] = do
  left <- unpacker arg1
  right <- unpacker arg2
  return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

strBoolBinopCaseInsensitive :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinopCaseInsensitive f = strBoolBinop (f `on` strToLower)
  where
    strToLower = unpack . toLower . pack

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params <&> Number . foldl1 op
