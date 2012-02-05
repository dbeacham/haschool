module DataTypeParser where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad (liftM)
import Numeric (readHex, readOct, readFloat)
import Ratio (Ratio, (%))
import Complex (Complex(..))
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
             | Float Double
             | Rational (Ratio Integer)
             | Complex (Complex Double)
             | String String
             | Bool Bool
             | Character Char
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
  return . Atom $ [first] ++ rest

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

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  val <- try $ string "newline" <|> string "space"
      <|> do { c <- anyChar; notFollowedBy alphaNum; return [c] }
  return $ Character $ case val of
    "newline" -> '\n'
    "space"   -> ' '
    otherwise -> val !! 0

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float $ fst . head $ readFloat (x ++ "." ++ y)

parseRational :: Parser LispVal
parseRational = do
  num <- many1 digit
  char '/'
  den <- many1 digit
  return . Rational $ (read num) % (read den)
  
parseComplex :: Parser LispVal
parseComplex = do
  x <- do { try parseFloat <|> parseNumber }
  char '+'
  y <- do { try parseFloat <|> parseNumber }
  char 'i'
  return . Complex $ toDouble x :+ toDouble y

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n ) = fromIntegral n
toDouble _ = undefined

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        -- we need the 'try' as they can all start with a hash charater
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRational
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> try parseDottedList
               char ')'
               return x
