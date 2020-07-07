module Parser where

import           Data.List
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
parseChar = Char <$> (string "#\\" >> anyChar)

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- parseEscaped <|> many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

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
decimal = Number . read <$> many1 digit
octal = Number . parseOct <$> ((string "#o" <|> string "#O") >> many1 (oneOf "01234567"))
hexadecimal = Number . parseHex <$> ((string "#x" <|> string "#X") >> many1 (oneOf "0123456789abcdef"))
binary = Number . parseBinary <$> ((string "#b" <|> string "#B") >> many1 (oneOf "01"))

-- TODO: negatives
parseFloat :: Parser LispVal
parseFloat = Float . (\x -> read x :: Float) <$> do
    int <- many1 digit
    dot <- char '.'
    dec <- many1 digit
    return $ int ++ [dot] ++ dec

parsePolar :: Parser LispVal
parsePolar = do
    Float r <- parseFloat
    sep <- char '@'
    Float theta <- parseFloat
    return $ Complex $ Polar r theta

parseCartesian :: Parser LispVal
parseCartesian = do
    Float r <- parseFloat
    plusminus <- oneOf "+-"
    Float i <- parseFloat
    char 'i'
    return $ Complex $ Cart r (if plusminus == '-' then -i else i)

parseComplex :: Parser LispVal
parseComplex = try parsePolar <|> parseCartesian

parseExpr :: Parser LispVal
parseExpr = parseChar
         <|> parseComplex
         <|> parseFloat
         <|> parseNumber
         <|> parseAtom
         <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> show val
