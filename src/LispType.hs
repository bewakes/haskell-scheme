module LispType where

import Lisp
import LispError

checkIfNumber :: LispVal -> Either LispVal Bool
checkIfNumber (LInteger _) = return True
checkIfNumber (LFloat _) = return True
checkIfNumber (LRational _) = return True
checkIfNumber nonNumber = Left nonNumber

checkAllNumbers :: [LispVal] -> Either LispVal Bool
checkAllNumbers = foldr ((>>) . checkIfNumber) (return True)

lTrue = LBool True
lFalse = LBool False
-- Type checking
--
isSymbol, isNumber, isString, isFloat, isChar, isComplex, isRational, isList, isInteger :: [LispVal] -> ThrowsError LispVal
isSymbol [LAtom _] = return lTrue
isSymbol _ = return lFalse

isChar [LChar _] = return lTrue
isChar _ = return lFalse

isNumber [LInteger _] = return lTrue
isNumber [LRational _] = return lTrue
isNumber [LFloat _] = return lTrue
isNumber _ = return lFalse

isString [LString _] = return lTrue
isString _ = return lFalse

isInteger [LInteger _] = return lTrue
isInteger _ = return lFalse

isFloat [LFloat _] = return lTrue
isFloat _ = return lFalse

isComplex [LComplex _] = return lTrue
isComplex _ = return lFalse

isRational [LRational _] = return lTrue
isRational _ = return lFalse

isList [LList _] = return lTrue
isList [LDottedList _ _] = return lTrue
isList _ = return lFalse
