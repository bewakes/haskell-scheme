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

instance Num LispVal where
    (LInteger i) + (LInteger j) = LInteger (i + j)
    (LInteger i) + (LFloat j) = LFloat (fromInteger i + j)
    (LFloat i) + (LInteger j) = LFloat (i + fromInteger j)
    (LInteger i) + (LRational j) = LRational (fromInteger i + j)
    (LRational i) + (LInteger j) = LRational (i + fromInteger j)

    (LInteger i) * (LInteger j) = LInteger (i * j)
    (LInteger i) * (LFloat j) = LFloat (fromInteger i * j)
    (LFloat i) * (LInteger j) = LFloat (i * fromInteger j)
    (LInteger i) * (LRational j) = LRational (fromInteger i * j)
    (LRational i) * (LInteger j) = LRational (i * fromInteger j)

    (LInteger i) - (LInteger j) = LInteger (i - j)
    (LInteger i) - (LFloat j) = LFloat (fromInteger i - j)
    (LFloat i) - (LInteger j) = LFloat (i - fromInteger j)
    (LInteger i) - (LRational j) = LRational (fromInteger i - j)
    (LRational i) - (LInteger j) = LRational (i - fromInteger j)

instance Integral LispVal where
    div (LInteger i) (LInteger j) = LInteger $ div i j
    rem (LInteger i) (LInteger j) = LInteger $ rem i j
    quot (LInteger i) (LInteger j) = LInteger $ quot i j

instance Enum LispVal where
    fromEnum (LInteger x) = fromInteger x
    toEnum x = LInteger (fromIntegral x)

instance Eq LispVal where
    (LInteger x) == (LInteger y) = x == y
    (LFloat x) == (LInteger y) = x == fromInteger y
    (LInteger y) == (LFloat x) = x == fromInteger y
    (LRational x) == (LInteger y) = x == fromInteger y
    (LInteger y) == (LRational x) = x == fromInteger y

    (LRational x) == (LFloat y) = x == toRational y
    (LFloat y) == (LRational x) = x == toRational y

instance Ord LispVal where
    compare (LInteger x) (LInteger y) = compare x y
    compare (LFloat x) (LInteger y) = compare x (fromInteger y)
    compare (LInteger x) (LFloat y) = compare (fromInteger x) y
    compare (LRational x) (LInteger y) = compare x (fromInteger y)
    compare (LInteger x) (LRational y) = compare (fromInteger x) y

    compare (LRational x) (LFloat y) = compare x (toRational y)
    compare (LFloat x) (LRational y) = compare (toRational x) y

instance Real LispVal where
    toRational (LInteger x) = toRational (x % 1)


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
showVal (LComplex (Cart r t)) = showVal r ++ "+" ++ showVal t ++ "i"  -- HANDLE NEGATIVE
showVal (LChar c) = "#\\" ++ [c]
showVal (LList contents) = "(" ++ unwordsList contents ++ ")"
showVal (LVector contents) = "#(" ++ unwordsList contents ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal
