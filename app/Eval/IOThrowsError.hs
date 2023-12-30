module Eval.IOThrowsError (IOThrowsError, liftThrows, runIOThrows) where

import Control.Monad.Except
import Data.Functor
import Eval.LispError (LispError)
import Eval.ThrowsError

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right value) = return value

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue
