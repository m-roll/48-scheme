module Core.IOThrowsError (IOThrowsError, liftThrows, runIOThrows) where

import Control.Monad.Except
import Core.LispError ()
import Core.ThrowsError
import Core.Type (IOThrowsError)
import Data.Functor

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right value) = return value

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue
