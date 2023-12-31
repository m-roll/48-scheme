module Core.Env (Env, NewBinding, nullEnv, isBound, getVar, setVar, defineVar, bindVars) where

import Control.Monad.Except
import Core.LispError
import Core.Type
import Data.Functor
import Data.IORef
import Data.Maybe

type NewBinding = (String, LispVal)

nullEnv :: IO Env
nullEnv = newIORef []

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
