module Lisp where

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
             deriving Show

negateLispReal :: LispVal -> LispVal
negateLispReal (LInteger x) = LInteger (-x)
negateLispReal (LFloat x) = LFloat (-x)
negateLispReal (LRational x) = LRational (-x)
