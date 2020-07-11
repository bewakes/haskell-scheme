module Lisp where

import Data.Ratio

data Complex = Polar LispVal LispVal
             | Cart LispVal LispVal
             deriving Show

data LispVal = LAtom String
             | LList [LispVal]
             | LVector [LispVal]
             | LDottedList [LispVal] LispVal
             | LString String
             | LBool Bool
             | LChar Char
             | LInteger Integer
             | LFloat Float
             | LRational Rational
             | LComplex Complex

negateLispReal :: LispVal -> LispVal
negateLispReal (LInteger x) = LInteger (-x)
negateLispReal (LFloat x) = LFloat (-x)
negateLispReal (LRational x) = LRational (-x)

showVal :: LispVal -> String
showVal (LString contents) = "\"" ++ contents ++ "\""
showVal (LAtom name) = name
showVal (LBool True) = "#t"
showVal (LBool False) = "#f"
showVal (LInteger contents) = show contents
showVal (LFloat contents) = show contents
showVal (LRational contents) = show (numerator contents) ++ "/" ++ show (denominator contents)
showVal (LComplex (Polar r t)) = showVal r ++ "@" ++ showVal t
showVal (LComplex (Cart r t)) = showVal r ++ "+" ++ showVal t  -- HANDLE NEGATIVE
showVal (LChar c) = "#\\" ++ [c]
showVal (LList contents) = "(" ++ unwordsList contents ++ ")"
showVal (LVector contents) = "#(" ++ unwordsList contents ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal
