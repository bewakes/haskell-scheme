module Lisp where

data Complex = Cart Float Float
             | Polar Float Float
             deriving (Show, Eq)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             | Float Float
             | Complex Complex
             deriving Show
