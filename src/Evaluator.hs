module Evaluator where

import           Control.Monad
import           Control.Monad.Except
import           Lisp
import           LispError
import           LispType

eval :: LispVal -> ThrowsError LispVal
eval val@(LString _)               = return val
eval val@(LAtom _)               = return val
eval val@(LBool _)                 = return val
eval val@(LInteger _)              = return val
eval val@(LRational _)             = return val
eval val@(LChar _)                 = return val
eval val@(LFloat _)                = return val
eval val@(LComplex _)              = return val
eval val@(LList [LAtom "quote", args]) = return val
eval (LList (LAtom "case": args)) = caseL args
eval (LList (LAtom func : args))   = mapM eval args >>= apply func
eval (LList xs)                    = do
        vals <- mapM eval xs
        return $ LList vals

apply :: String -> [LispVal] -> ThrowsError LispVal
-- TODO: enforce same type for conseq and alt
apply "if" [LBool True, conseq, _] = return conseq
apply "if" [LBool False, _, alt] = return alt
apply "if" unmatched = throwError $ NumArgs 3 unmatched
apply "else" [elseVal] = return $ LList [LAtom "else", elseVal]

-- rest of the functions
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
             -- List Primitives
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             -- Equivalences
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", eqv)  -- NOTE: Ommiting weak checking for now
             , ("cond", cond)
             -- , ("case", caseL)
             ]

-- TODO: Symbol Handling function ??
type Unpacker a = LispVal -> ThrowsError a

unpackString :: LispVal -> ThrowsError String
unpackString (LString s) = return s
unpackString nonString   = throwError $ TypeMismatch "string" nonString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LBool b) = return b
unpackBool nonBool   = throwError $ TypeMismatch "boolean" nonBool

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

-- List primitives
car :: [LispVal] -> ThrowsError LispVal
car [LList (x: xs)]         = return x
car [LDottedList (x: xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgsList             = throwError $ NumArgs 1 badArgsList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [LList (x: xs)]        = return $ LList xs
cdr [LDottedList [_] x]    = return x
cdr [LDottedList (_:xs) x] = return $ LDottedList xs x
cdr [badArg]               = throwError $ TypeMismatch "pair" badArg
cdr badArgsList            = throwError $ NumArgs 1 badArgsList

cons :: [LispVal] -> ThrowsError LispVal
cons [LAtom x, _]       = throwError $ BadSpecialForm "Unrecognized special form" (LAtom x)
cons [x1, LList []]            = return $ LList [x1]
cons [x1, LList [LAtom "quote", LList []]]            = return $ LList [x1]
-- cons [quoted@(LList (LAtom "quote": xs)), LList []]            = return quoted
cons [x, LList xs]             = return $ LList $ x: xs
cons [x, LDottedList xs xlast] = return $ LDottedList (x:xs) xlast
cons [x1, x2]                  = return $ LDottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [LBool a, LBool b] = return $ LBool $ a == b
eqv [LChar a, LChar b] = return $ LBool $ a == b
eqv [LString a, LString b] = return $ LBool $ a == b
eqv [LAtom a, LAtom b] = return $ LBool $ a == b
eqv [LList a, LList b] = return $ LBool $ (length a == length b) && all eqvPair (zip a b)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left _            -> False
                               Right (LBool val) -> val

eqv [a, b] = return $ case checkAllNumbers [a, b] of
               Left _  -> LBool False
               Right _ -> LBool $ a == b
eqv badArgList = throwError $ NumArgs  2 badArgList

cond :: [LispVal] -> ThrowsError LispVal
cond [LList []] = return $ LList []
cond [LList [LAtom "else", elseVal]] = return elseVal  -- this is matching condition for else
cond (LList [LBool True, val]: _) = return val
cond (LList [LBool False, _]: restCond) = cond restCond
cond (x:xs) = throwError $ BadSpecialForm "Invalid arg for `cond`" x

caseL :: [LispVal] -> ThrowsError LispVal
caseL (lst@(LList _): datumList) = eval lst >>= (`checkValueForDatumMatch` datumList)
    where checkValueForDatumMatch :: LispVal -> [LispVal] -> ThrowsError LispVal
          checkValueForDatumMatch value [LList [LAtom "else", val]] = return val
          checkValueForDatumMatch value (LList [LList datum, val]: remainingDatum) = if isValueInList value datum
                                                                                  then return val
                                                                                  else checkValueForDatumMatch value remainingDatum
          checkValueForDatumMatch _ [] = throwError $ BadSpecialForm "Invalid number of args for `case`" (LList [])
          checkValueForDatumMatch _ invalidArgs = throwError $ BadSpecialForm "Invalid args for `case`" (LList invalidArgs)
          isValueInList lispVal = foldr (\x a -> if a then a else case eqv [lispVal, x] of Right (LBool True) -> True; Right (LBool False) -> False) False
