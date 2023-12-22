module Eval.Env where

import Ast (LispVal)
import Control.Monad.Except
import Data.Functor
import Data.IORef
import Data.Maybe
import Eval.LispError

-- BEGIN IO plumbing
-- Move to another module.

type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right value) = return value

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

-- END IO plumbing.

type NewBinding = (String, LispVal)

type EnvBinding = (String, IORef LispVal)

-- This type should really be called 'EnvRef'
type Env = IORef [EnvBinding]

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (`writeIORef` value))
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
  where
    extendEnv :: [EnvBinding] -> IO [EnvBinding]
    extendEnv env = fmap (++ env) (mapM addBinding bindings)
    addBinding :: NewBinding -> IO EnvBinding
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
