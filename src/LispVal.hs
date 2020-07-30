{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module LispVal where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Text            as T
import           Data.Typeable        (Typeable)

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
    deriving ( Monad
             , Functor
             , Applicative
             , MonadReader EnvCtx
             , MonadIO
             )

data LispVal
    = Atom T.Text
    | List [LispVal]
    | Number Integer
    | String T.Text
    | Fun IFunc
    | Lambda IFunc EnvCtx
    | Nil
    | Bool Bool
    deriving (Typeable)

instance Eq LispVal where
    Number a == Number b = a == b
    Atom a == Atom b = a == b
    String a == String b = a == b
    Bool a == Bool b = a == b

    Nil == Nil = True
    _ == Nil = False
    Nil == _ = False

newtype IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal (Atom atom)  = atom
showVal (String str) = T.concat ["\"", str, "\""]
showVal (Number num) = T.pack $ show num
showVal (Bool True)  = "#t"
showVal (Bool False) = "#f"
showVal Nil          = "Nil"
showVal (Fun _)      = "(internal function)"
showVal (Lambda _ _) = "(lambda function)"
showVal (List contents) = T.concat ["(", T.unwords $ showVal <$> contents, ")"]
