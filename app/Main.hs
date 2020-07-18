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
  let statements = lines fileContent
      evaled = map evalString statements
  mapM_ print $ zipWith (\x y -> x ++ " ---> " ++ y) statements evaled
