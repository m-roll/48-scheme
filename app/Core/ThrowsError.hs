module Core.ThrowsError (ThrowsError, trapError, extractValue) where

import Control.Monad.Except
import Core.Type (LispError)

type ThrowsError = Either LispError

-- trapError :: ThrowsError String -> ThrowsError String
trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
