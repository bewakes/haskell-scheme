module Evaluator where

import           Control.Monad
import           Control.Monad.Except
import           Data.IORef
import           Data.Maybe
import           Lisp
import           LispError
import           LispType

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) =  return val

nullEnv :: IO Env
nullEnv = newIORef []

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Got an unbound var" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envref var value = do
    env <- liftIO $ readIORef envref
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (`writeIORef` value))
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value
    else liftIO $ do
        valueRef <- newIORef value
        env <- readIORef envRef
        writeIORef envRef ((var, valueRef) : env)
        return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = (++ env) <$> mapM addBinding bindings
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(LString _)               = return val
eval env val@(LAtom id)               = getVar env id
eval env val@(LBool _)                 = return val
eval env val@(LInteger _)              = return val
eval env val@(LRational _)             = return val
eval env val@(LChar _)                 = return val
eval env val@(LFloat _)                = return val
eval env val@(LComplex _)              = return val
eval env val@(LList [LAtom "quote", args]) = return val
eval env (LList [LAtom "if", pred, conseq, alt]) =
    do
        result <- eval env pred
        case result of
          LBool False -> eval env alt
          _           -> eval env conseq

eval env (LList [LAtom "case", caseValExpr]) = return $ LList []
eval env (LList [LAtom "case", caseValExpr, LList [LAtom "else", val]]) = eval env val
eval env (LList (LAtom "case": caseValExpr: dataSeqsResults)) =
    do
        val <- eval env caseValExpr
        checkCase val dataSeqsResults
    where checkCase val [] = return $ LList []
          checkCase val [LList [LAtom "else", res]] = eval env res
          checkCase val (LList [LList dataSeq, res]: xs) = do
              evaledVals <- mapM (eval env) dataSeq
              let found = any (areEquivalent val) evaledVals
              if found then eval env res
                       else checkCase val xs

eval env (LList [LAtom "set!", LAtom var, form]) =
    eval env form >>= setVar env var
eval env (LList [LAtom "define", LAtom var, form]) =
    eval env form >>= defineVar env var
eval env (LList (LAtom func : args))   = mapM (eval env) args >>= liftThrows . apply func
-- eval env (LList (LAtom "case": args)) = caseL args
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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


areEquivalent :: LispVal -> LispVal -> Bool
areEquivalent a b = case eqv [a, b] of
    Left _              -> False
    Right ( LBool True) -> True
    _                   -> False

eqv :: [LispVal] -> ThrowsError LispVal
eqv [LBool a, LBool b] = return $ LBool $ a == b
eqv [LChar a, LChar b] = return $ LBool $ a == b
eqv [LString a, LString b] = return $ LBool $ a == b
eqv [LAtom a, LAtom b] = return $ LBool $ a == b
eqv [LList a, LList b] = return $ LBool $ (length a == length b) && and (zipWith areEquivalent a b)

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
