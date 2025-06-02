module Main where

import Lexer
import Parser
import Emitter
import System.Environment (getArgs)

main = do
 arg:_ <- getArgs
 file <- readFile $ arg
 let code = toC' $ toProgramTree [] $ getTokens file
 writeFile "teeny.c" code
