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
             ]

numericBinOp :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> LispVal
numericBinOp = foldl1
