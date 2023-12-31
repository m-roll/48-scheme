module Core.Equality (eqv, equal, eqvHelper) where

import Control.Monad.Except
import Control.Monad.Identity
import Core.Coercion
import Core.LispError
import Core.LispVal
import Core.ThrowsError

eqv :: [LispVal] -> ThrowsError LispVal
eqv [arg1, arg2] = return . Bool $ eqvHelper arg1 arg2
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvHelper :: LispVal -> LispVal -> Bool
eqvHelper (Bool arg1') (Bool arg2') = arg1' == arg2'
eqvHelper (Number arg1') (Number arg2') = arg1' == arg2'
eqvHelper (String arg1') (String arg2') = arg1' == arg2'
eqvHelper (Atom arg1') (Atom arg2') = arg1' == arg2'
eqvHelper (DottedList xs x) (DottedList ys y) = eqvList xs' ys'
  where
    xs' = xs ++ [x]
    ys' = ys ++ [y]
eqvHelper (List xs) (List ys) = eqvList xs ys
-- TODO equality of functions
eqvHelper _ _ = False

eqvList :: [LispVal] -> [LispVal] -> Bool
eqvList xs ys = runIdentity $ listEquality eqvHelperM xs ys
  where
    eqvHelperM x y = Identity $ eqvHelper x y

-- This could probably be expressed more idiomatically.
-- The recursive part is fine, but there's no reason to only allow monad inputs here.
-- Perhaps some combination of lifting could help here.
listEquality :: Monad m => (LispVal -> LispVal -> m Bool) -> [LispVal] -> [LispVal] -> m Bool
listEquality eqChecker (x : xs) (y : ys) = do
  e <- eqChecker x y
  e' <- listEquality eqChecker xs ys
  return (e && e')
listEquality _ [] [] = return True
listEquality _ _ _ = return False

equal :: [LispVal] -> ThrowsError LispVal
equal [List xs, List ys] = fmap Bool (listEquality equalHelper xs ys)
equal [arg1, arg2] = fmap Bool (equalHelper arg1 arg2)
equal badArgList = throwError $ NumArgs 2 badArgList

equalHelper :: LispVal -> LispVal -> ThrowsError Bool
equalHelper arg1 arg2 = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return (primitiveEquals || let (Bool x) = eqvEquals in x) -- I don't like this non-exhaustive pattern matching
