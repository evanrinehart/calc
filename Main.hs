module Main where

import System.Environment
import Control.Exception
import System.Exit
import Expr
import Eval
import Parser
import Infer
import C


main = do
  args <- getArgs
  e0 <- case args of
    [] -> do
      src <- getContents
      return (parse "stdin" src)
    ("-c":path:_) -> do
      e <- parseFile path
      let code = compile e
      putStrLn code
      exitSuccess
    (path:_) -> parseFile path
  t <- evaluate (infer e0)
  let e1 = eval e0
  putStrLn (show e1 ++ " : " ++ show t)
