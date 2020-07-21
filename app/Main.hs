module Main where

import           Control.Monad
import           Control.Monad.Except
import           System.IO

import           Evaluator
import           Lisp
import           LispError
import           Parser

evalString :: Env -> String -> IO String
evalString env x = runIOThrows $ show <$> (liftThrows (readExpr x) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

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

runOne :: String -> IO ()
runOne expr = nullEnv >>= (`evalAndPrint` expr)


runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "> ") . evalAndPrint

runFile :: IO ()
runFile = do
  fileContent <- readFile "programs/program.sch"
  env <- nullEnv
  let statements = lines fileContent
  lst <- mapM (evalString env) statements
  mapM_ print $ zipWith (\x y -> x ++ " ---> " ++ y) statements lst

main :: IO ()
main = runRepl
