module Eval.LispError (LispError (..), ThrowsError, trapError, extractValue) where -- shouldnt be tied to eval since parsing errors here too

import Ast
import Ast.Print
import Control.Monad.Except (MonadError, catchError)
import Text.Parsec

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | CaseNotMatched
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default err) = err
showError CaseNotMatched = "Case not matched"

instance Show LispError where show = showError

type ThrowsError = Either LispError

-- trapError :: ThrowsError String -> ThrowsError String
trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
