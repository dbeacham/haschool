module DataTypeParser where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad (liftM)
import Numeric (readHex, readOct)
import Data.Char (digitToInt)

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
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

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
parseAtom = liftM Atom $ many1 $ letter <|> digit <|> symbol

parseNumber :: Parser LispVal
-- liftM version
-- parseNumber = liftM (Number . read) $ many1 digit

-- Ex1a. do-notation version
-- parseNumber = do
--  nbrStr <- many1 digit
--  return $ Number $ read nbrStr

-- Ex1b. explicit monad form
-- parseNumber = many1 digit >>= \s -> return $ Number $ read s

-- Ex4. Extend to support numbers of different bases
parseNumber = do
  parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseDigital1 :: Parser LispVal
parseDigital1 = liftM (Number . read) $ many1 digit

parseDigital2 :: Parser LispVal
parseDigital2 = do
  try $ string "#d"
  liftM (Number . read) $ many1 digit

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dig x)

hex2dig x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dig x)

oct2dig x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 $ oneOf "01"
  return $ Number (bin2dig x)

bin2dig :: String -> Integer
bin2dig str = fromIntegral $ foldl ((+) . (2*)) 0 $ map digitToInt str

parseBool = do
  char '#'
  x <- oneOf "tf"
  case x of
    't' -> return $ Bool True
    'f' -> return $ Bool False

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseBool
