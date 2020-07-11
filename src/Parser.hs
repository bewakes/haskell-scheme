module Parser where

import           Data.List
import           Data.Ratio
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

import           Lisp

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

hash :: Parser Char
hash = char '#'

spaces :: Parser ()
spaces = skipMany1 space

parseEscaped :: Parser String
parseEscaped = do
    e <- char '\\'
    c <- oneOf "\"nrt\\"
    return [e, c]

parseCharNoHash :: Parser LispVal
parseCharNoHash = LChar <$> (string "#\\" >> anyChar)

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

parseDecimalIntStr :: Parser String
parseDecimalIntStr = many digit

parseFloatStr :: Parser String
parseFloatStr = (\a b c -> a ++ [b] ++ c) <$> parseDecimalIntStr <*> char '.' <*> parseDecimalIntStr

parseHex, parseOct, parseBinary :: String -> Integer
parseHex hexStr = sum $ zipWith (\p val -> round (intVal val * 16 ** p)) [0..] (reverse hexStr)
    where intVal s = case elemIndex s hexAlphabet of
                       Just x -> fromIntegral x
                       _      -> error "Invalid hex alphabet"
          hexAlphabet = "0123456789abcdef"

parseOct octStr = sum $ zipWith (\p val -> round (read [val] * 8 ** p)) [0..] (reverse octStr)
parseBinary binStr = sum $ zipWith (\p val -> round (read [val] * 2 ** p)) [0..] (reverse binStr)

parseDecimalSuffix = (++) <$> string "." <*> parseDecimalIntStr
parseRationalSuffix = (++) <$> string "/" <*> parseDecimalIntStr

-- TODO: negatives
parseReal :: Parser LispVal
parseReal = do
    number <- parseDecimalIntStr
    rest <- parseDecimalSuffix <|> parseRationalSuffix <|> return ""
    let getType intStr "" = LInteger $ read number
        getType intStr ('.':decStr) = LFloat $ read (intStr ++ "." ++ decStr)
        getType intStr ('/':denStr) = LRational $ toRational $ read intStr  % read denStr
        in return $ getType number rest

---Parsers for diffrent number bases
parseOctalNoHash, parseHexNoHash, parseBinaryNoHash:: Parser LispVal
parseOctalNoHash = LInteger . parseOct <$> ((string "o" <|> string "O") >> many1 (oneOf "01234567"))
parseHexNoHash = LInteger . parseHex <$> ((string "x" <|> string "X") >> many1 (oneOf "0123456789abcdef"))
parseBinaryNoHash = LInteger . parseBinary <$> ((string "b" <|> string "B") >> many1 (oneOf "01"))

parseComplex :: Parser LispVal
parseComplex = do
    pref <- parseReal
    sep <- oneOf "+-@"
    suff <- parseReal
    let getComplex p '@' s = LComplex $ Polar p s
        getComplex p '+' s = LComplex $ Cart p s
        getComplex p '-' s = LComplex $ Cart p (negateLispReal s)
     in return $ getComplex pref sep suff

parseExpr :: Parser LispVal
parseExpr = try (hash >> (parseOctalNoHash <|> parseHexNoHash <|> parseBinaryNoHash <|> parseCharNoHash <|> parseVectorNoHash))
         <|> parseAtom
         <|> try parseComplex
         <|> parseReal
         <|> parseString
         <|> parseQuoted
         <|> parseCommaList
         <|> parseBackTicked
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser LispVal
parseList = LList <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ LDottedList head tail

-- Single quote syntactic sugar of Scheme
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ LList [LAtom "quote", x]

parseBackTicked :: Parser LispVal
parseBackTicked = do
    char '`'
    x <- parseExpr
    return $ LList [LAtom "back-tick", x]

parseCommaList :: Parser LispVal
parseCommaList = do
    char ','
    x <- parseExpr
    return $ LList [LAtom "comma", x]

parseVectorNoHash :: Parser LispVal
parseVectorNoHash = do
    char '('
    x <- sepBy parseExpr spaces
    char ')'
    return $ LVector x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> show val
