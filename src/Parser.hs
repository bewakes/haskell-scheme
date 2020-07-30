{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative   hiding ((<|>))
import           Data.Functor.Identity (Identity)
import qualified Data.Text             as T
import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Language  as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tok

import           LispVal

-- newtype Parser a = Parser (T.Text -> [(a, T.Text)])

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
    {
        Tok.commentStart = "{-"
      , Tok.commentEnd = "-}"
      , Tok.opStart = Tok.opLetter style
      , Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
      , Tok.identStart = letter <|> oneOf "-+/*=|&><"
      , Tok.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
      , Tok.reservedOpNames = [ "'", "\""]
    }

Tok.TokenParser { Tok.parens = m_parens
            , Tok.identifier = m_identifier } = Tok.makeTokenParser style

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = Atom . T.pack <$> m_identifier

parseText :: Parser LispVal
parseText = do
    reservedOp "\""
    p <- many1 $ noneOf "\""
    reservedOp "\""
    return $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber = parseNegNum <|> Number . read <$> many1 digit

parseNegNum :: Parser LispVal
parseNegNum = do
    char '-'
    d <- many1 digit
    return $ Number . negate . read $ d

whiteSpace :: Parser ()
whiteSpace = do
    _ <- char ' ' <|> char '\n'
    return ()

parseList :: Parser LispVal
parseList = List . concat <$> Text.Parsec.many parseExpr `sepBy` whiteSpace

parseSExp = List. concat <$> m_parens (Text.Parsec.many parseExpr `sepBy` whiteSpace)

parseQuote :: Parser LispVal
parseQuote = do
    reservedOp "\'"
    x <- parseExpr
    return $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved =
    (reservedOp "Nil" >> return Nil)
    <|> (reservedOp "#t" >> return (Bool True))
    <|> (reservedOp "#f" >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr = parseReserved
    <|> parseNumber
    <|> parseAtom
    <|> parseText
    <|> parseQuote
    <|> parseSExp

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"
