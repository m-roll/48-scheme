module Eval.IOThrowsError (IOThrowsError, liftThrows, runIOThrows) where

import Control.Monad.Except
import Data.Functor
import Eval.LispError ()
import Eval.ThrowsError
import Eval.Type (IOThrowsError)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right value) = return value

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue
