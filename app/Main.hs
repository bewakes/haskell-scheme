module Main where

import Control.Monad
import System.IO

import Lisp
import LispError
import Parser
import Evaluator

evalString :: String -> String
evalString x = extractValue $ trapError $ show <$> (readExpr x >>= eval)

main :: IO ()
main = do
    fileContent <- readFile "programs/program.sch"
    let evaled = map evalString . lines $ fileContent
    print evaled
