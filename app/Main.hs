module Main where

import           Control.Monad
import           System.IO

import           Evaluator
import           Lisp
import           LispError
import           Parser

evalString :: String -> String
evalString x = extractValue $ trapError $ show <$> (readExpr x >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = do
    let evaled = evalString expr
    putStrLn evaled

-- print a string and immediately flush the stream, otherwise output might sit in output buffers
-- and user will never see prompts or results
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
       then return ()
       else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

runFile :: IO ()
runFile = do
  fileContent <- readFile "programs/program.sch"
  let statements = lines fileContent
      evaled = map evalString statements
  mapM_ print $ zipWith (\x y -> x ++ " ---> " ++ y) statements evaled

main :: IO ()
main = runRepl
