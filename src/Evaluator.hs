module Evaluator where

import Lisp

eval :: LispVal -> LispVal
eval val@(LString _) = val
eval val@(LBool _) = val
eval val@(LInteger _) = val
eval val@(LRational _) = val
eval val@(LChar _) = val
eval val@(LFloat _) = val
eval val@(LComplex _) = val
eval (LList [LAtom "quote", val]) = val
eval (LList (LAtom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (LBool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
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
             ]

numericBinOp :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> LispVal
numericBinOp = foldl1

lTrue = LBool True
lFalse = LBool False
-- Type checking
checkSymbol, checkNumber, checkString, checkFloat, checkComplex, checkRational, checkList, checkInteger :: [LispVal] -> LispVal
checkSymbol [LAtom _] = lTrue
checkSymbol _ = lFalse

checkNumber [LInteger _] = lTrue
checkNumber [LRational _] = lTrue
checkNumber [LFloat _] = lTrue
checkNumber _ = lFalse

checkString [LString _] = lTrue
checkString _ = lFalse

checkInteger [LInteger _] = lTrue
checkInteger _ = lFalse

checkFloat [LFloat _] = lTrue
checkFloat _ = lFalse

checkComplex [LComplex _] = lTrue
checkComplex _ = lFalse

checkRational [LRational _] = lTrue
checkRational _ = lFalse

checkList [LList _] = lTrue
checkList [LDottedList _ _] = lTrue
checkList _ = lFalse
