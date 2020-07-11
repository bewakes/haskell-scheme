module Main where

import System.IO

import Lisp
import Parser
import Evaluator


main :: IO ()
main = do
    fileContent <- readFile "program.sch"
    print . map (eval . readExpr) . lines $ fileContent
