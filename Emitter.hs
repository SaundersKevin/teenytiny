module Emitter (toC', toC) where

import Tokens
import ProgramTree
import System.IO
import Data.List

toC':: ProgramTree -> Code
toC' tree = 
 let (code,var) = toC ([],[]) tree
 in  "#include <stdio.h>\nint main() { float " ++ (unwords (intersperse "," var)) ++ ";" ++ code

toC :: (Code, Variables) -> ProgramTree -> (Code, Variables)
toC (code, var) [] = (code ++ "}",var)

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
     addVar  = ident
 in  toC (code ++ addCode, addVar:var) tree

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
