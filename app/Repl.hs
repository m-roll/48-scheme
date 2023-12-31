module Repl (runRepl, runOne) where

import Core (eval, readExpr)
import Eval.Env
import Eval.IOThrowsError
import Eval.LispVal
import Eval.Primitives
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows (fmap show $ liftThrows (readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= flip
      bindVars
      allPrimatives
  where
    makePrimitiveFunc constructor (var, func) = (var, constructor func)
    allPrimatives =
      fmap (makePrimitiveFunc PrimitiveFunc) primitives
        ++ fmap (makePrimitiveFunc IOFunc) ioPrimitives

-- instead of flipping, it kinda makes more sense to have env as a right param for most functions.
-- This way the functions can be curried and passed around with fewer changes
runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action = do
  result <- prompt
  if pred' result
    then return ()
    else action result >> until_ pred' prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
