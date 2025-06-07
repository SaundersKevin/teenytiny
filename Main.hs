module Main where

import Lexer
import Set
import Parser
import Emitter
import System.Environment (getArgs)

main = do
 arg:_ <- getArgs
 file <- readFile $ arg
 let code = toC' $ toProgramTree [] $ parseSet $ getTokens file
 writeFile "teeny.c" code
