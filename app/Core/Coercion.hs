{-# LANGUAGE ExistentialQuantification #-}

module Core.Coercion (Unpacker (AnyUnpacker), unpackEquals, unpackNum, unpackBool, unpackStr, unpackChar) where

import Control.Monad.Except (catchError, throwError)
import Core.LispError (LispError (..))
import Core.LispVal
import Core.ThrowsError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` const (return False)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum = u
  where
    u (Number n) = return n
    u (String n) =
      let parsed = reads n
       in if null parsed
            then throwError . TypeMismatch "number" $ String n
            else return $ fst $ head parsed
    -- u (List [n]) = unpackNum n
    u notNum = throwError . TypeMismatch "number" $ notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool = u
  where
    u (Bool b) = return b
    u notBool = throwError . TypeMismatch "bool" $ notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr = u
  where
    u (String s) = return s
    u (Number n) = return $ show n
    u (Bool b) = return $ show b
    u notStr = throwError . TypeMismatch "string" $ notStr

unpackChar :: LispVal -> ThrowsError Char
unpackChar = u
  where
    u (Char c) = return c
    u notChar = throwError . TypeMismatch "char" $ notChar
