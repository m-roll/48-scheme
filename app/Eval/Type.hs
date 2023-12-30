module Eval.Type (LispVal (..), Env, EnvBinding, ThrowsError, LispError (..)) where

import Data.IORef
import Text.Parsec (ParseError)

-- `a' is not in the book. The data definitions are mutually recursive,
-- and my module structure reflects that. This parameterizes the LispVal
-- type over some error type
--
-- Another fix here is to just not indicate the PrimitiveFunc is
-- fallible in the LispVal def. Long term, that makes more sense,
-- since the fallibility of functions is unrelated to the data
-- and feels un-schemey

type EnvBinding = (String, IORef LispVal)

type ThrowsError = Either LispError

-- This type should really be called 'EnvRef'
-- Also, we can definitely create an env without using global mutable state. I'm actually not sure this is gonna work gracefully with closures
type Env = IORef [EnvBinding]

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  | Float Float
  | Vector [LispVal]
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | CaseNotMatched
  | Default String
