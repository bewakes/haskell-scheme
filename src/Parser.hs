module Parser where

import           Data.List
import           Data.Ratio
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

import           Lisp

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseEscaped :: Parser String
parseEscaped = do
    e <- char '\\'
    c <- oneOf "\"nrt\\"
    return [e, c]

parseChar :: Parser LispVal
parseChar = LChar <$> (string "#\\" >> anyChar)

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- parseEscaped <|> many (noneOf "\"")
    char '"'
    return $ LString x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> LBool True
               "#f" -> LBool False
               _    -> LAtom atom

parseHex, parseOct, parseBinary :: String -> Integer
parseHex hexStr = sum $ zipWith (\p val -> round (intVal val * 16 ** p)) [0..] (reverse hexStr)
    where intVal s = case elemIndex s hexAlphabet of
                       Just x -> fromIntegral x
                       _      -> error "Invalid hex alphabet"
          hexAlphabet = "0123456789abcdef"

parseOct octStr = sum $ zipWith (\p val -> round (read [val] * 8 ** p)) [0..] (reverse octStr)
parseBinary binStr = sum $ zipWith (\p val -> round (read [val] * 2 ** p)) [0..] (reverse binStr)

--TODO: negatives
parseNumber :: Parser LispVal
parseNumber = try hexadecimal <|> try octal <|> decimal

---Parsers for diffrent number bases
decimal, octal, hexadecimal, binary :: Parser LispVal
decimal = LInteger . read <$> many1 digit
octal = LInteger . parseOct <$> ((string "#o" <|> string "#O") >> many1 (oneOf "01234567"))
hexadecimal = LInteger . parseHex <$> ((string "#x" <|> string "#X") >> many1 (oneOf "0123456789abcdef"))
binary = LInteger . parseBinary <$> ((string "#b" <|> string "#B") >> many1 (oneOf "01"))

-- TODO: negatives
parseFloat :: Parser LispVal
parseFloat = LFloat . (\x -> read x :: Float) <$> do
    int <- many1 digit
    dot <- char '.'
    dec <- many1 digit
    return $ int ++ [dot] ++ dec

parsePolar :: Parser LispVal
parsePolar = do
    mag <- try parseRational <|> try parseFloat <|> parseNumber
    sep <- char '@'
    theta <- try parseRational <|> try parseFloat <|> parseNumber
    return $ LComplex $ Polar mag theta

parseCartesian :: Parser LispVal
parseCartesian = do
    real <- try parseRational <|> try parseFloat <|> parseNumber
    plusminus <- oneOf "+-"
    img <- try parseRational <|> try parseFloat <|> parseNumber
    char 'i'
    return $ LComplex $ Cart real (if plusminus == '-' then negateLispReal img else img)

parseComplex :: Parser LispVal
parseComplex = try parsePolar <|> parseCartesian

parseRational :: Parser LispVal
parseRational = do
    (LInteger num) <- decimal
    char '/'
    (LInteger den) <- decimal
    return $ LRational $ toRational (num % den)

parseExpr :: Parser LispVal
parseExpr = parseChar
         <|> try parseComplex
         <|> try parseRational
         <|> try parseFloat
         <|> parseNumber
         <|> parseAtom
         <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> show val
