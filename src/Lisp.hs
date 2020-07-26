module Lisp where

import           Control.Monad.Except
import           Data.IORef
import           Data.Ratio
import           Text.Parsec.Error

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
             | LPrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | LFunc {
                       params :: [String]
                     , vararg :: Maybe String
                     , body :: [LispVal], closure :: Env
                     }


type Env = IORef [(String, IORef LispVal)]

-- LISP errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


-- Define a type to represent functions that may throw a LispError or return a value
type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

-- We convert all our errors to their string representations and return that as normal value
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- INSTANCES

instance Num LispVal where
    (LInteger i) + (LInteger j) = LInteger (i + j)
    (LInteger i) + (LFloat j) = LFloat (fromInteger i + j)
    (LFloat i) + (LInteger j) = LFloat (i + fromInteger j)
    (LInteger i) + (LRational j) = LRational (fromInteger i + j)
    (LRational i) + (LInteger j) = LRational (i + fromInteger j)
    (LRational i) + (LFloat j) = LRational (i + toRational j)
    (LFloat i) + (LRational j) = LRational (toRational i + j)
    (LFloat x) * (LFloat y) = LFloat (x * y)
    (LRational x) * (LRational y) = LRational (x * y)

    (LInteger i) * (LInteger j) = LInteger (i * j)
    (LInteger i) * (LFloat j) = LFloat (fromInteger i * j)
    (LFloat i) * (LInteger j) = LFloat (i * fromInteger j)
    (LInteger i) * (LRational j) = LRational (fromInteger i * j)
    (LRational i) * (LInteger j) = LRational (i * fromInteger j)
    (LFloat x) * (LRational y) = LRational (toRational x * y)
    (LRational x) * (LFloat y) = LRational (x * toRational y)

    (LInteger i) - (LInteger j) = LInteger (i - j)
    (LInteger i) - (LFloat j) = LFloat (fromInteger i - j)
    (LFloat i) - (LInteger j) = LFloat (i - fromInteger j)
    (LInteger i) - (LRational j) = LRational (fromInteger i - j)
    (LRational i) - (LInteger j) = LRational (i - fromInteger j)
    (LFloat x) - (LRational y) = LRational (toRational x - y)
    (LRational x) - (LFloat y) = LRational (x - toRational y)
    (LFloat x) - (LFloat y) = LFloat (x - y)
    (LRational x) - (LRational y) = LRational (x - y)


instance Integral LispVal where
    div (LInteger i) (LInteger j) = LInteger $ div i j
    rem (LInteger i) (LInteger j) = LInteger $ rem i j
    quot (LInteger i) (LInteger j) = LInteger $ quot i j

instance Enum LispVal where
    fromEnum (LInteger x) = fromInteger x
    toEnum x = LInteger (fromIntegral x)

instance Fractional LispVal where
    (LInteger x) / (LInteger y) = LRational (x % y)
    (LFloat x) / (LFloat y) = LFloat (x / y)
    (LRational x) / (LRational y) = LRational (x / y)
    (LInteger x) / (LFloat y) = LFloat (fromInteger x / y)
    (LFloat x) / (LInteger y) = LFloat (x / fromInteger y)
    (LRational x) / (LInteger y) = LRational (x / fromInteger y)
    (LInteger x) / (LRational y) = LRational (fromInteger x / y)
    (LFloat x) / (LRational y) = LRational (toRational x / y)
    (LRational x) / (LFloat y) = LRational (x / toRational y)

instance Eq LispVal where
    (LInteger x) == (LInteger y) = x == y
    (LFloat x) == (LInteger y) = x == fromInteger y
    (LInteger y) == (LFloat x) = x == fromInteger y
    (LRational x) == (LInteger y) = x == fromInteger y
    (LInteger y) == (LRational x) = x == fromInteger y

    (LRational x) == (LFloat y) = x == toRational y
    (LFloat y) == (LRational x) = x == toRational y

instance Ord LispVal where
    compare (LInteger x) (LInteger y)  = compare x y
    compare (LFloat x) (LInteger y)    = compare x (fromInteger y)
    compare (LInteger x) (LFloat y)    = compare (fromInteger x) y
    compare (LRational x) (LInteger y) = compare x (fromInteger y)
    compare (LInteger x) (LRational y) = compare (fromInteger x) y

    compare (LRational x) (LFloat y)   = compare x (toRational y)
    compare (LFloat x) (LRational y)   = compare (toRational x) y

instance Real LispVal where
    toRational (LInteger x) = toRational (x % 1)


negateLispReal :: LispVal -> LispVal
negateLispReal (LInteger x)  = LInteger (-x)
negateLispReal (LFloat x)    = LFloat (-x)
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
showVal (LComplex (Cart r t)) = showVal r ++ (if t >= LFloat 0 then "+" else "") ++ showVal t ++ "i"  -- HANDLE NEGATIVE
showVal (LChar c) = "#\\" ++ [c]
showVal (LList (LAtom "quote": rest)) = "'" ++ unwordsList rest
showVal (LList contents) = "(" ++ unwordsList contents ++ ")"
showVal (LDottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
showVal (LVector contents) = "#(" ++ unwordsList contents ++ ")"
showVal (LPrimitiveFunc _) = "<primitive>"
showVal LFunc {params = args, vararg = varargs, body = body, closure = env} =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal
