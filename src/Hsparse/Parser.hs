module Hsparse.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Complex
import Data.Char
import Data.Ratio
import Data.Array
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Bool Bool
             | Number Integer 
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | Real Float
             | Character Char
             | String String

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++"\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

symbol :: Parser Char
symbol = oneOf "!$%&|*+-:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                   "#t" -> Bool True
                   "#f" -> Bool False
                   _    -> Atom atom

parseBool :: Parser LispVal
parseBool = do char '#'
               x <- oneOf "tf"
               return $ case x of
                't' -> Bool True
                'f' -> Bool False


parseRational :: Parser LispVal
parseRational = do numerator <- many1(digit)
                   char '/'
                   denominator <- many1(digit)
                   return $ Ratio ((read numerator) % read(denominator))

parseComplex :: Parser LispVal
parseComplex = do real <- (try parseFloat <|> parseNumber)
                  char '+'
                  imag <- (try parseFloat <|> parseNumber)
                  char 'i'
                  return $ Complex (toDouble real :+ toDouble imag)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseBinary :: Parser LispVal
parseBinary = do try $ string "#b"
                 x <- many1 (oneOf "01")
                 return $ Number (bin2dig x)


parseOctal :: Parser LispVal
parseOctal = do try $ string "#o"
                x <- many octDigit
                return $ Number (oct2dig x)

parseHex :: Parser LispVal
parseHex = do try $ string "#h"
              x <- many hexDigit
              return $ Number (hex2dig x)

parsePrefixDecimal :: Parser LispVal
parsePrefixDecimal = do try $ string "#d"
                        x <- many digit
                        (return . Number . read) x

parseDecimal :: Parser LispVal
parseDecimal = do x <- many1 digit
                  (return . Number . read) x


parseNumber :: Parser LispVal
--parseNumber = do x <- many1 digit
--                 return $ Number $ read x
--parseNumber = many1 digit >>= \x -> return $ Number $ read x
--parseNumber = do x <- many1 $ (digit <|> char '#' ) 
--parseNumber = do first <- string "#b" <|> string "#o" <|> string "#d" <|> string "#h" <|> many1(digit)
--                 rest <- many1 (digit)
--                 return $ case first of
--                    "#b" -> Number . fst $ head $ readInt 2 ((<2).digitToInt) digitToInt rest
--                    "#o" -> Number . fst $ head $ readOct rest
--                    "#d" -> Number . fst $ head $ readDec rest
--                    "#h" -> Number . fst $ head $ readHex rest
--                    _    -> Number . fst $ head $ readDec first
parseNumber = do num <- parsePrefixDecimal <|> parseDecimal <|> parseHex <|> parseOctal <|> parseBinary
                 return $ num

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\'    -> x
                    '"'     -> x
                    'n'     -> '\n'
                    'r'     -> '\r'
                    't'     -> '\t'
                    '0'     -> '\0'

parseCharacter :: Parser LispVal
parseCharacter = do first <- string "#\\"
                    rest <- many1 (anyChar) 
                    let char = head rest
                    return $ case rest of
                        "space"     -> Character ' '
                        "newline"   -> Character '\n'
                        " "         -> Character '\0'
                        x:xs        -> Character x
                        _           -> Character '\0' 

parseString :: Parser LispVal
parseString = do char '"'
                 --x <- many (char '\"' <|> noneOf "\"")
                 --x <- many $ (noneOf "\"" <|> oneOf "\"")
                 --x <- many $ (alphaNum <|> char '\"' <|> newline <|> tab 
                 --               <|> char '\r' <|> char '\\' )
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseFloat :: Parser LispVal
parseFloat = do first       <- many1 (digit)
                char '.'
                rest        <- many1 (digit)
                return $ Float . fromRational $ fst $ head $ readFloat (first ++ "." ++ rest) 

parseUnquote :: Parser LispVal
parseUnquote = do char ','
                  x <- parseExpr
                  return $ x

-- @TODO: not working
parseUnquoteSplice :: Parser LispVal
parseUnquoteSplice = do
    string ",@"
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "unquote", x]

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
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseRational
    <|> parseNumber
    <|> parseQuoted
    <|> parseQuasiQuote
    <|> parseUnquote
    <|> parseBool
    <|> parseFloat
    <|> parseCharacter
    <|> do char '('
           x <- try parseList <|> parseDottedList
           char ')'
           return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

