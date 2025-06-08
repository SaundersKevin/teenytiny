module Main where

import Lexer
import Set
import Parser
import Emitter
import System.Environment (getArgs)

main = do
 args <- getArgs
 if args == []
  then putStrLn "No filename provided"
  else compile $ head args

compile :: String -> IO ()
compile arg = do
 file <- readFile arg
 let code = toC' $ toProgramTree [] $ parseSet $ getTokens file
 writeFile "teeny.c" code
