module Main where

import Lexer
import Parser
import Emitter
import System.Environment (getArgs)

main = do
 arg:args <- getArgs
 file <- readFile $ arg
 tokens = getTokens file
 emit $ parse tokens
