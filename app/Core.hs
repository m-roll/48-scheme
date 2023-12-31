module Core where

import Control.Monad.Except
import Core.Env
import Core.Equality
import Core.IOThrowsError
import Core.LispError
import Core.LispVal
import Core.ThrowsError
import Data.Maybe
import LispParser
import Text.ParserCombinators.Parsec hiding (spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom id') = getVar env id'
eval env (List (id'@(Atom name) : rest)) =
  case evalSpecialForm env name rest of
    Just specialFormResult -> specialFormResult
    Nothing -> do
      fn <- eval env id'
      argVals <- mapM (eval env) rest
      apply fn argVals
eval _ val = return val

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply badType _ = throwError $ TypeMismatch "function or primitive" badType

evalSpecialForm :: Env -> String -> [LispVal] -> Maybe (IOThrowsError LispVal)
evalSpecialForm _ "quote" [val] = return $ return val
evalSpecialForm env "if" [pred', conseq, alt] = return $ ifSpecialForm env pred' conseq alt
evalSpecialForm env "set!" [Atom var, form] = return $ eval env form >>= setVar env var
evalSpecialForm env "case" rest = return $ caseSpecialForm env rest
evalSpecialForm env "cond" rest = return $ condSpecialForm env rest
evalSpecialForm env "define" [Atom var, form] =
  return $ eval env form >>= defineVar env var
evalSpecialForm env "define" (List (Atom var : params) : body) =
  return $ makeNormalFunc env params body >>= defineVar env var
evalSpecialForm env "define" (DottedList (Atom var : params) varargs : body) =
  return $ makeVarArgs varargs env params body >>= defineVar env var
evalSpecialForm env "lambda" (List params : body) =
  return $ makeNormalFunc env params body
evalSpecialForm env "lambda" (DottedList params varargs : body) =
  return $ makeVarArgs varargs env params body
evalSpecialForm env "lambda" (varargs@(Atom _) : body) =
  return $ makeVarArgs varargs env [] body
evalSpecialForm env "load" [String filename] =
  return $ load filename >>= fmap last . mapM (eval env)
evalSpecialForm _ _ _ = Nothing

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

ifSpecialForm :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
ifSpecialForm env pred' conseq alt = do
  result <- eval env pred'
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    badArg -> throwError $ TypeMismatch "bool" badArg

-- TODO implement `else`
condSpecialForm :: Env -> [LispVal] -> IOThrowsError LispVal
condSpecialForm env = evalCaseP
  where
    evalCaseP :: [LispVal] -> IOThrowsError LispVal
    evalCaseP (List [pred', conseq] : rest) = do
      result <- eval env pred'
      case result of
        Bool True -> eval env conseq
        Bool False -> evalCaseP rest
        badArg -> throwError $ TypeMismatch "bool" badArg
    evalCaseP (badForm : _) = throwError $ BadSpecialForm "unrecognized case form" badForm
    evalCaseP [] = throwError CaseNotMatched

caseSpecialForm :: Env -> [LispVal] -> IOThrowsError LispVal
caseSpecialForm env (key : allClauses) = do
  keyResult <- eval env key
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
caseSpecialForm env [] = undefined

-- TODO: Why is there no validation against bad bindingis? We just `show` the param expr with no validation
makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show
