module Evaluator where

import Control.Monad
import Control.Monad.Except
import Lisp
import LispError

eval :: LispVal -> ThrowsError LispVal
eval val@(LString _) = return val
eval val@(LBool _) = return val
eval val@(LInteger _) = return val
eval val@(LRational _) = return val
eval val@(LChar _) = return val
eval val@(LFloat _) = return val
eval val@(LComplex _) = return val
eval (LList (LAtom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinOp (+))
             , ("-", numericBinOp (-))
             , ("*", numericBinOp (*))
             , ("/", numericBinOp (/))
             , ("mod", numericBinOp mod)
             , ("quotient", numericBinOp quot)
             , ("remainder", numericBinOp rem)
             -- Type checking
             , ("symbol?", checkSymbol)
             , ("number?", checkNumber)
             , ("integer?", checkInteger)
             , ("string?", checkString)
             , ("float?", checkFloat)
             , ("complex?", checkComplex)
             , ("rational?", checkRational)
             , ("list?", checkList)
             , ("char?", checkChar)
             ]

-- TODO: Symbol Handling function ??

numericBinOp :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
numericBinOp func [] = throwError $ NumArgs 2 []
numericBinOp func val@[_] = throwError $ NumArgs 2 val
numericBinOp func vals = return $ foldl1 func vals

lTrue = LBool True
lFalse = LBool False
-- Type checking
checkSymbol, checkNumber, checkString, checkFloat, checkChar, checkComplex, checkRational, checkList, checkInteger :: [LispVal] -> ThrowsError LispVal
checkSymbol [LAtom _] = return lTrue
checkSymbol _ = return lFalse

checkChar [LChar _] = return lTrue
checkChar _ = return lFalse

checkNumber [LInteger _] = return lTrue
checkNumber [LRational _] = return lTrue
checkNumber [LFloat _] = return lTrue
checkNumber _ = return lFalse

checkString [LString _] = return lTrue
checkString _ = return lFalse

checkInteger [LInteger _] = return lTrue
checkInteger _ = return lFalse

checkFloat [LFloat _] = return lTrue
checkFloat _ = return lFalse

checkComplex [LComplex _] = return lTrue
checkComplex _ = return lFalse

checkRational [LRational _] = return lTrue
checkRational _ = return lFalse

checkList [LList _] = return lTrue
checkList [LDottedList _ _] = return lTrue
checkList _ = return lFalse
