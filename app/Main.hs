module Main where

import           Control.Monad
import           System.IO

import           Evaluator
import           Lisp
import           LispError
import           Parser

evalString :: String -> String
evalString x = extractValue $ trapError $ show <$> (readExpr x >>= eval)

main :: IO ()
main = do
  fileContent <- readFile "programs/program.sch"
  let evaled = map evalString . lines $ fileContent
  print evaled
