import           Control.Monad()
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)
import Ast
import Eval

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "No match: " ++ show err
  Right val -> val

