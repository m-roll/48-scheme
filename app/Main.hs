import           Control.Monad()
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)
import Ast

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found " ++ show val

