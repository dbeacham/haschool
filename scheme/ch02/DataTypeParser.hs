module DataTypeParser where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
import Control.Monad.Error
import Numeric (readHex, readOct, readFloat)
import Data.Ratio (Ratio, (%))
import Data.Complex (Complex(..))
import Data.Char (digitToInt)

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
  
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Complex (r :+ i)) = show r ++ "+" ++ show i ++ "i"
showVal (Float contents) = show contents
showVal (Rational contents) = show contents
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where
  show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args.\nFound values: " ++ show found
showError (TypeMismatch expected found) = "Expected value of type " ++ show expected ++ "\nFound: " ++ show found
showError (Parser parseErr) = "Parser error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)  = message ++ ": " ++ show func
showError (UnboundVar message varName) = message ++ ": " ++ varName

instance Show LispError where
  show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal 

eval :: LispVal -> ThrowsError LispVal
-- primitive types
eval val@(Atom _) = return val
eval val@(String _) = return val
eval val@(Complex _) = return val
eval val@(Float _) = return val
eval val@(Rational _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    otherwise -> eval conseq
eval (List [Atom "quote", val]) = return val
-- function application
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognised primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("/=", numBoolBinop (/=))
  , ("<", numBoolBinop (<))
  , ("<=", numBoolBinop (<=))
  , (">", numBoolBinop (>))
  , ("=>", numBoolBinop (>=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  case length args /= 2 of
    True -> throwError $ NumArgs 2 args
    False -> do left <- unpacker $ args !! 0
                right <- unpacker $ args !! 1
                return . Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "Number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList
