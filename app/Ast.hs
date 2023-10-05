module Ast (parseExpr, Show, module Ast.Type)
where

import Ast.Type
import Ast.Instance()
import Control.Monad()
import Data.Functor
import Data.List
import Numeric
import Text.Parsec.Char hiding (spaces)
import Text.ParserCombinators.Parsec hiding (spaces)
-- https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4
-- TODO: handle special cases:
--   #\space
--   #\newline

parseLists :: Parser LispVal
parseLists = parentheses (do
  es <- parseExprSeq
  option (List es) (parseDottedListPostfix <&> DottedList es))

parseDottedListPostfix :: Parser LispVal
parseDottedListPostfix = char '.' >> spaces >>  parseExpr

parentheses :: Parser a -> Parser a
parentheses = between (char '(') (char ')')

parseExprSeq :: Parser [LispVal]
parseExprSeq = sepEndBy parseExpr spaces

parseVector :: Parser LispVal
parseVector = do
  _ <- char '#'
  Vector <$> parentheses (sepBy parseExpr spaces)

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal 
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr -- technically, should not be an <expression> but a  <qq template>
  return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  _ <- char ','
  _ <- char '@'
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseChar :: Parser LispVal
parseChar = do
  _ <- string' "#\\"
  c <- anyChar
  return . Char $ c

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many stringChar
  _ <- char '"'
  return $ String x

stringChar :: Parser Char
stringChar = noneOf "\"" <|> escapeChar

-- exercise 2/3
escapeChar :: Parser Char
escapeChar = isoEscapeChar '\\'
  <|> isoEscapeChar '\"'
  <|> specialEscapeChar 'n' '\n'
  <|> specialEscapeChar 'r' '\r'
  <|> specialEscapeChar 't' '\t'

isoEscapeChar :: Char -> Parser Char
isoEscapeChar e = specialEscapeChar e e

specialEscapeChar :: Char -> Char -> Parser Char
specialEscapeChar e c = char '\\' >> char e >> return c

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

-- TODO support the full number tower, or look up how to do it. I'm confused here.

parseNumber :: Parser LispVal
parseNumber = parseInteger <|> parseFloats -- use try since . also used by dotted list, for backtracking

-- TODO: support s/f/d/l floats
-- s = short
-- f = single
-- d = double
-- l = long
--
-- TODO: This only handles x.xxx style inexact floats rn.
parseFloats :: Parser LispVal
parseFloats = do 
  preDecimal <- many1 digit
  decimal <- char '.'
  postDecimal <- many1 digit
  -- technically, out of spec of R5RS. Unmarked floats should default to Double precision or higher.
  float <- liftReadS readFloat (preDecimal ++ decimal : postDecimal)
  return . Float $ float

parseInteger :: Parser LispVal
parseInteger = (parseBinaryNumber
  <|> parseOctalNumber
  <|> parseDecimalNumberWithPrefix
  <|> parseHexadecimalNumber
  <|> parseDecimalNumber) <&> Number

-- from https://stackoverflow.com/questions/3568767/haskell-lifting-a-reads-function-to-a-parsec-parser
liftReadS :: ReadS a -> String -> Parser a
liftReadS reader = maybe (unexpected "no parse") (return . fst) .
                   find (null . snd) . reader

parseBinaryNumber :: Parser Integer
parseBinaryNumber = do
  _ <- string' "#b"
  numSeq <- many1 . oneOf $ "01"
  liftReadS readBin numSeq

parseOctalNumber :: Parser Integer
parseOctalNumber = do
  _ <- string' "#o"
  numSeq <- many1 . oneOf $ "01234567"
  liftReadS readOct numSeq

parseDecimalNumberWithPrefix :: Parser Integer
parseDecimalNumberWithPrefix = do
  _ <- string' "#d"
  parseDecimalNumber

parseHexadecimalNumber :: Parser Integer
parseHexadecimalNumber = do
  _ <- string' "#x"
  numSeq <- many1 $ digit <|> oneOf "abcdef"
  liftReadS readHex numSeq

parseDecimalNumber:: Parser Integer
parseDecimalNumber = read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseChar
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> parseUnquoted
  <|> parseUnquoteSplicing
  <|> parseLists
  <|> parseVector

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space
