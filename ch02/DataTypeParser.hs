module DataTypeParser where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad (liftM)

main :: IO ()
main = do
  (arg:_) <- getArgs
  putStrLn $ readExpr $ arg

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

-- Ex2,3. R5Rs allows for internally escaped quotes
escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '"' -> x
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
  deriving Show

parseString :: Parser LispVal
parseString = do
  char '"'
  -- non-R5RS compliant
  -- x <- many (noneOf "\"")
  -- Ex2,3. R5Rs allows for internally escaped quotes and special characters
  x <- many $ escapedChars <|> noneOf "\""
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = [first] ++ rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             otherwise -> Atom atom

parseNumber :: Parser LispVal
-- liftM version
parseNumber = liftM (Number . read) $ many1 digit

-- Ex1a. do-notation version
-- parseNumber = do
--  nbrStr <- many1 digit
--  return $ Number $ read nbrStr

-- Ex1b. explicit monad form
-- parseNumber = many1 digit >>= \s -> return $ Number $ read s

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
