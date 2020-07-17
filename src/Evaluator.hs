module Evaluator where

import           Control.Monad
import           Control.Monad.Except
import           Lisp
import           LispError
import           LispType

eval :: LispVal -> ThrowsError LispVal
eval val@(LString _)             = return val
eval val@(LBool _)               = return val
eval val@(LInteger _)            = return val
eval val@(LRational _)           = return val
eval val@(LChar _)               = return val
eval val@(LFloat _)              = return val
eval val@(LComplex _)            = return val
eval (LList (LAtom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
               ("+", numericBinOp (+))
             , ("-", numericBinOp (-))
             , ("*", numericBinOp (*))
             , ("/", numericBinOp (/))
             , ("mod", numericBinOp mod)
             , ("quotient", numericBinOp quot)
             , ("remainder", numericBinOp rem)
             -- Numeric binary boolean operations
             , ("=", numericBoolBinOp (==))
             , ("<", numericBoolBinOp (<))
             , (">", numericBoolBinOp (>))
             , (">=", numericBoolBinOp (>=))
             , ("<=", numericBoolBinOp (<=))
             , ("/=", numericBoolBinOp (/=))
             -- Boolean binary operations
             , ("&&", boolBoolBinOp (&&))
             , ("||", boolBoolBinOp (||))
             -- String boolean binary ops
             , ("string=?", stringBoolBinOp (==))
             , ("string<?", stringBoolBinOp (<))
             , ("string>?", stringBoolBinOp (>))
             , ("string<=?", stringBoolBinOp (<=))
             , ("string>=?", stringBoolBinOp (>=))
             -- Type checking
             , ("symbol?", isSymbol)
             , ("number?", isNumber)
             , ("integer?", isInteger)
             , ("string?", isString)
             , ("float?", isFloat)
             , ("complex?", isComplex)
             , ("rational?", isRational)
             , ("list?", isList)
             , ("char?", isChar)
             ]

-- TODO: Symbol Handling function ??
type Unpacker a = LispVal -> ThrowsError a

unpackString :: LispVal -> ThrowsError String
unpackString (LString s) = return s
unpackString nonString   = throwError $ TypeMismatch "string" nonString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LBool b) = return b
unpackBool nonBool = throwError $ TypeMismatch "boolean" nonBool

-- NOTE: maybe need to implement unpackNum
numericBoolBinOp :: (LispVal -> LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
numericBoolBinOp op lst@[x, xs] = case checkAllNumbers lst of
                                       Right True -> return $ LBool $ op x xs
                                       Left val -> throwError $ TypeMismatch "number" val
numericBoolBinOp op misMatcArgs = throwError $ NumArgs 2 misMatcArgs

stringBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
stringBoolBinOp = boolBinOp unpackString

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

boolBinOp :: Unpacker a -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpack op [opr1, opr2] = do
    op1 <- unpack opr1
    op2 <- unpack opr2
    return $ LBool $ op1 `op` op2
boolBinOp _ op oprs = throwError $ NumArgs 2 oprs

numericBinOp :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
numericBinOp func [] = throwError $ NumArgs 2 []
numericBinOp func val@[_] = throwError $ NumArgs 2 val
numericBinOp func vals = case checkAllNumbers vals of
                           Right True -> return $ foldl1 func vals
                           Left val -> throwError $ TypeMismatch "number" val
