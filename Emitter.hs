module Emitter (toC') where

import Tokens
import ProgramTree
import System.IO
import Data.List

-- Runtime - setting up stack and variables -- Something like this for the stack?
runtime = "#include <stdio.h>\n#include <stdlib.h>\n#define STACK_SIZE 32\nint main(){float stack[STACK_SIZE];int stackpointer = 0;void push(float var)\n{if (stackpointer >= STACK_SIZE){printf(\"Stack overflow (push)\");exit(1);}stack[stackpointer] = var;stackpointer++;}float pop() {if (stackpointer == 0){printf(\"Stack underflow (pop)\");exit(1);}stackpointer--;return stack[stackpointer];}float peek() {if (stackpointer == 0){printf(\"Stack underflow (peek)\");exit(1);}return stack[stackpointer];}float "


toC':: ProgramTree -> Code
toC' tree = 
 let (code,var) = toC ([],[]) tree
 in  runtime ++ (unwords (intersperse "," var)) ++ ";" ++ code

toC :: (Code, Variables) -> ProgramTree -> (Code, Variables)
toC (code, var) [] = (code ++ "}",var)

-- STACK Functions --
-- POP Function --
toC (code, var) (Function POP ((Ident (Token ident IDENT)):[] ):tree) = 
 let addCode = ident ++ " = pop();"
     finalVar = if ident `elem` var then var else ident:var
 in  toC (code ++ addCode, finalVar) tree

-- PUSH Function --
toC (code, var) (Function PUSH ((Equation equation):[] ):tree) =
 let addCode = "push(" ++ (toEqC equation) ++ ");"
 in  toC (code ++ addCode, var) tree

-- PEEK Function --
toC (code, var) (Function PEEK ((Ident (Token ident IDENT)):[] ):tree) =
 let addCode = ident ++ " = peek();"
     finalVar = if ident `elem` var then var else ident:var
 in  toC (code ++ addCode, finalVar) tree

-- STRING Functions --
toC (code, var) (Function PRINT ((ParamString (Token str STRING)):[] ):tree) = 
 let addCode = "printf(\"" ++ str ++ "\\n\");"
 in  toC (code ++ addCode, var) tree
toC (code, var) ( Function PRINT ((Ident (Token ident IDENT)):[] ):tree) =
 let addCode = "printf( \"%.2f\\n\", (float)(" ++ ident ++ "));"
 in toC (code ++ addCode, var) tree

-- INPUT Function --
toC (code, var) ( Function INPUT ((Ident (Token ident IDENT)):[] ):tree) = 
 let addCode = "scanf(\"%f\", &" ++ ident ++ ");"
     finalVar  = if ident `elem` var then var else ident:var
 in  toC (code ++ addCode, finalVar) tree

-- LABEL Function --
toC (code, var) ( Function LABEL ((Ident (Token ident IDENT)):[] ):tree) =
 let addCode = ident ++ ":"
 in toC (code ++ addCode, var) tree

-- GOTO Function --
toC (code, var) ( Function GOTO ((Ident (Token ident IDENT)):[] ):tree) =
 let addCode = "goto " ++ ident ++ ";"
 in toC (code ++ addCode, var) tree

-- WHILE Function --
toC (code, var) ( Function WHILE ((Equation eq1):(Compare compare):(Equation eq2):(Tree whileTree):[] ):tree) =
 let (whileCode, addVar) = toC ([],var) whileTree
     equ1 = toEqC eq1
     equ2 = toEqC eq2
     (Token comp _) = compare
     addCode = "while (" ++ equ1 ++ comp ++ equ2 ++ ") {" ++ whileCode
     returnVar = if addVar /= [""] then nub (addVar ++ var) else var
 in  toC (code ++ addCode, returnVar) tree

-- IF Function --
toC (code, var) ( Function IF ((Equation eq1):(Compare compare):(Equation eq2):(Tree ifTree):[] ):tree) =
 let (ifCode, addVar) = toC ([],var) ifTree
     equ1 = toEqC eq1
     equ2 = toEqC eq2
     (Token comp _) = compare
     addCode = "if (" ++ equ1 ++ comp ++ equ2 ++ ") {" ++ ifCode
     returnVar = if addVar /= [""] then nub (addVar ++ var) else var
 in  toC (code ++ addCode, returnVar) tree

-- LET Function --
toC (code, var) (( Function LET (( Ident (Token ident IDENT)):(Equation equation):[]) ):tree) =
 let equationC = toEqC equation
     addCode = ident ++ "=" ++ equationC ++ ";"
     finalVar = if ident `elem` var then var else ident:var
 in toC (code ++ addCode, finalVar) tree

-- Helper Functions --
toEqC :: [Token] -> String
toEqC eq = unwords $ map getData eq
